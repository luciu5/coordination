test_that("Stackelberg Logit calibrates audited Bertrand equations", {
  fit <- stackelberg(
    prices = c(10, 12, 11, 9),
    shares = c(.25, .20, .18, .17),
    margins = c(.40, .38, .35, .25),
    ownerPre = c("A", "B", "C", "D"),
    leadersPre = "A", insideSize = 1000
  )
  expect_s4_class(fit, "StackelbergLogit")
  expect_true(isTRUE(methods::is(fit, "Logit")))
  expect_equal(unname(fit@pricePre), unname(fit@prices), tolerance = 1e-7)
  expect_lt(stackelberg_residuals(fit)$max, 1e-6)
  expect_equal(unname(calcMC(fit, TRUE)), unname(fit@mcPre))
  expect_equal(unname(calcMC(fit, FALSE)), unname(fit@mcPost))
  expect_equal(unname(calcShares(fit, TRUE)), unname(fit@shares), tolerance = 1e-10)
  expect_equal(unname(calcQuantities(fit, TRUE)), unname(fit@mktSize * fit@shares), tolerance = 1e-10)
  fast <- stackelberg(
    prices = c(10, 12, 11, 9), shares = c(.25, .20, .18, .17),
    margins = c(.40, .38, .35, .25), ownerPre = c("A", "B", "C", "D"),
    leadersPre = "A", insideSize = 1000,
    control.equ = list(implicitCheck = FALSE)
  )
  expect_equal(fast@diagnostics$implicit$statusPre, "not-run")
  expect_true(all(is.na(fast@diagnostics$implicit$pre)))
})

test_that("Cournot and input sign cases retain positive actions", {
  for (conduct in c("bertrand", "cournot")) {
    fit <- stackelberg(
      prices = c(10, 12, 11, 9), shares = c(.25, .20, .18, .17),
      margins = c(.4, .38, .35, .25), ownerPre = c("A", "B", "C", "D"),
      leadersPre = "A", conduct = conduct, output = FALSE,
      alpha = 2, insideSize = 1000
    )
    expect_true(all(fit@pricePre > 0))
    expect_true(all(calcMargins(fit, TRUE) > 0))
    expect_lt(stackelberg_residuals(fit)$max, 1e-6)
    expect_equal(calcPrices(fit, TRUE, method = "implicit"), fit@pricePre,
                 tolerance = 1e-4)
  }
})

test_that("WLS uses all usable partial margins and fixed alpha bypasses calibration", {
  prices <- c(10, 12, 11, 9)
  shares <- c(.25, .20, .18, .17)
  owner <- c("A", "B", "C", "D")
  ## Independent audited h values for one leader and three followers.
  fs <- shares
  B <- sum(fs[-1]^2 / (1 - fs[-1] + fs[-1]^2))
  h <- c(1 / (1 - (1 / (1 - B)) * fs[1]), 1 / (1 - fs[-1]))
  obs <- c(.4, .38, NA, .25)
  w <- c(1, 2, 3, 4)
  lambda <- sum(w[is.finite(obs)] * (h / prices)[is.finite(obs)] * obs[is.finite(obs)]) /
    sum(w[is.finite(obs)] * (h / prices)[is.finite(obs)]^2)
  noisy <- stackelberg(prices, shares, obs, ownerPre = owner,
                       leadersPre = "A", weights = w, insideSize = 100)
  expect_equal(noisy@diagnostics$demandparam, 1 / lambda, tolerance = 1e-10)

  fit <- stackelberg(
    prices = c(10, 12, 11), shares = c(.20, .18, .17),
    margins = c(.2, NA, .1), ownerPre = c("A", "B", "C"),
    leadersPre = "A", alpha = 2, insideSize = 100
  )
  expect_equal(fit@diagnostics$demandparam, 2)
  expect_equal(fit@diagnostics$observedMargins, c(.2, NA, .1))
  expect_true(all(is.finite(fit@mcPre)))
  expect_warning(validObject(fit), "ownerPost.*ownerPre")

  expect_error(
    stackelberg(c(10, 12), c(.2, .18), c(NA, NA),
                ownerPre = c("A", "B"), leadersPre = "A"),
    "all margins are missing"
  )
  fixed <- stackelberg(c(10, 12), c(.2, .18), c(NA, NA),
                       ownerPre = c("A", "B"), leadersPre = "A", alpha = 2)
  expect_equal(fixed@diagnostics$demandparam, 2)
  expect_true(all(is.finite(fixed@diagnostics$impliedMargins)))
})

test_that("follower reactions agree with finite differences and generic route", {
  for (conduct in c("bertrand", "cournot")) {
    fit <- stackelberg(c(10, 12, 11, 9), c(.25, .20, .18, .17),
                       c(.4, .38, .35, .25), ownerPre = c("A", "B", "C", "D"),
                       leadersPre = "A", conduct = conduct, insideSize = 1000)
    e <- 1e-3
    if (conduct == "bertrand") {
      yp <- stackelberg_followers(fit, 10 + e)$prices[-1]
      ym <- stackelberg_followers(fit, 10 - e)$prices[-1]
    } else {
      yp <- stackelberg_followers(fit, 250 + e)$quantities[-1]
      ym <- stackelberg_followers(fit, 250 - e)$quantities[-1]
    }
    expect_equal(as.numeric((yp - ym) / (2 * e)),
                 as.numeric(stackelberg_response(fit)), tolerance = 1e-5)
    expect_equal(as.numeric(stackelberg_response(fit, method = "implicit")),
                 as.numeric(stackelberg_response(fit)), tolerance = 1e-5)
  }
})

test_that("off-equilibrium leader reduced gradient has the payoff sign", {
  fit <- stackelberg(c(10, 12, 11, 9), c(.25, .20, .18, .17),
                     c(.4, .38, .35, .25), ownerPre = c("A", "B", "C", "D"),
                     leadersPre = "A", insideSize = 1000)
  e <- 1e-4
  candidate <- 10.3
  fprofit <- function(x) {
    ff <- stackelberg_followers(fit, x)
    ix <- seq_along(ff$prices) == 1
    sum((ff$prices[ix] - fit@mcPre[ix]) * ff$quantities[ix])
  }
  ff <- stackelberg_followers(fit, candidate)
  numericGradient <- (fprofit(candidate + e) - fprofit(candidate - e)) / (2 * e)
  reported <- stackelberg_residuals(fit, prices = ff$prices)$leader
  expect_equal(as.numeric(reported), numericGradient, tolerance = 1e-5)
})

test_that("empty leader and follower sectors have stable dimensions", {
  noLeaders <- stackelberg(c(10, 12), c(.2, .18), rep(NA, 2),
                           ownerPre = c("A", "B"), leadersPre = character(),
                           alpha = 2, insideSize = 100)
  expect_equal(dim(stackelberg_response(noLeaders)), c(2L, 0L))
  expect_length(stackelberg_residuals(noLeaders)$leader, 0L)

  allLeaders <- stackelberg(c(10, 12), c(.2, .18), rep(NA, 2),
                            ownerPre = c("A", "B"), leadersPre = c("A", "B"),
                            alpha = 2, insideSize = 100)
  expect_equal(dim(stackelberg_response(allLeaders)), c(0L, 2L))
  expect_length(stackelberg_residuals(allLeaders)$follower, 0L)
})

test_that("subset NA and Cournot no-follower domain are rejected", {
  expect_error(stackelberg(c(10, 12), c(.2, .18), c(.2, .2),
                           ownerPre = c("A", "B"), leadersPre = "A",
                           subset = c(TRUE, NA)), "subset")
  fit <- stackelberg(c(10, 12), c(.2, .18), rep(NA, 2),
                     ownerPre = c("A", "B"), leadersPre = c("A", "B"),
                     conduct = "cournot", alpha = 2, insideSize = 100)
  expect_error(stackelberg_followers(fit, c(263.2, 1)), "positive outside")
})

test_that("firm-level role partitions cover multiproduct and empty sectors", {
  prices <- c(10, 11, 12, 13, 14)
  shares <- c(.12, .11, .10, .09, .08)
  owners <- list(c("A", "A", "B", "C", "D"),
                 c("A", "B", "B", "C", "D"))
  leaders <- list("A", c("A", "B"), c("A", "B", "C", "D"), character())
  for (owner in owners) for (lead in leaders)
    for (conduct in c("bertrand", "cournot")) for (output in c(TRUE, FALSE)) {
      fit <- stackelberg(prices, shares, rep(NA_real_, 5), ownerPre = owner,
                         leadersPre = lead, conduct = conduct, output = output,
                         alpha = 1.4, insideSize = 100)
      expect_lt(stackelberg_residuals(fit)$max, 1e-7)
      expect_equal(calcPrices(fit, TRUE), fit@pricePre, tolerance = 1e-7)
      expect_true(all(is.finite(fit@pricePre)))
    }
})

test_that("one-leader reduced profit agrees with bounded direct optimization", {
  prices <- c(10, 11, 12, 13, 14)
  shares <- c(.12, .11, .10, .09, .08)
  owners <- c("A", "B", "C", "D", "E")
  for (conduct in c("bertrand", "cournot")) for (output in c(TRUE, FALSE)) {
    fit <- stackelberg(prices, shares, rep(NA_real_, 5), ownerPre = owners,
                       leadersPre = "A", conduct = conduct, output = output,
                       alpha = 1.4, insideSize = 100)
    li <- which(fit@firmOwnerPre %in% fit@leadersPre)
    objective <- function(action) {
      ff <- stackelberg_followers(fit, action)
      margin <- if (output) ff$prices[li] - fit@mcPre[li] else fit@mcPre[li] - ff$prices[li]
      -sum(margin * ff$quantities[li])
    }
    if (conduct == "bertrand") {
      interval <- c(7, 15)
      equilibriumAction <- fit@pricePre[li]
    } else {
      interval <- c(1, 50)
      equilibriumAction <- fit@mktSize * fit@shares[li]
    }
    direct <- stats::optimize(objective, interval = interval)
    ## The constructor's root must be the direct reduced-profit optimum;
    ## Brent's scalar stopping tolerance is looser than the FOC gate.
    expect_lt(abs(direct$objective - objective(equilibriumAction)), 1e-5)
    expect_equal(direct$objective, objective(equilibriumAction), tolerance = 1e-5)
  }
})

test_that("ownership-only, cost-only, and combined shocks solve both conducts", {
  prices <- c(10, 11, 12, 13, 14)
  shares <- c(.12, .11, .10, .09, .08)
  owners <- c("A", "B", "C", "D", "E")
  merged <- c("AB", "AB", "C", "D", "E")
  delta <- c(.05, 0, 0, 0, 0)
  for (conduct in c("bertrand", "cournot")) for (output in c(TRUE, FALSE)) {
    fit <- stackelberg(prices, shares, rep(NA_real_, 5), ownerPre = owners,
                       leadersPre = "A", conduct = conduct, output = output,
                       alpha = 1.4, insideSize = 100)
    ownOnly <- stackelberg_simulate(fit, ownerPost = merged, leadersPost = "AB")
    costOnly <- stackelberg_simulate(fit, mcDelta = delta)
    combined <- stackelberg_simulate(fit, ownerPost = merged, leadersPost = "AB",
                                      mcDelta = delta)
    expect_lt(stackelberg_residuals(ownOnly, FALSE)$max, 1e-7)
    expect_lt(stackelberg_residuals(costOnly, FALSE)$max, 1e-7)
    expect_lt(stackelberg_residuals(combined, FALSE)$max, 1e-7)
    expect_equal(costOnly@mcPre, fit@mcPre)
    expect_equal(costOnly@mcPost, fit@mcPre * (1 + delta))
  }
})

test_that("merger and cost shocks require explicit post roles", {
  fit <- stackelberg(c(10, 12, 11, 9), c(.25, .20, .18, .17), rep(NA, 4),
                     ownerPre = c("A", "B", "C", "D"), leadersPre = "A",
                     alpha = 2, insideSize = 1000)
  expect_error(stackelberg_simulate(fit, ownerPost = c("AB", "AB", "C", "D")),
               "leadersPost must be supplied")
  out <- stackelberg_simulate(
    fit, ownerPost = c("AB", "AB", "C", "D"), leadersPost = "AB",
    mcDelta = c(.1, 0, 0, 0)
  )
  expect_equal(out@leadersPost, "AB")
  expect_equal(out@mcPost, fit@mcPre * c(1.1, 1, 1, 1))
  expect_true(all(is.finite(out@pricePost)))
  expect_lt(stackelberg_residuals(out, FALSE)$max, 1e-6)
  exit <- stackelberg_simulate(fit, subset = c(TRUE, TRUE, FALSE, TRUE))
  expect_equal(unname(calcPrices(exit, FALSE, subset = rep(TRUE, 4))),
               unname(fit@pricePre), tolerance = 1e-7)
  expect_equal(unname(calcPrices(exit, FALSE, subset = rep(TRUE, 4), method = "implicit")),
               unname(fit@pricePre), tolerance = 1e-6)
})

test_that("unsupported normalizations and control matrices are explicit", {
  expect_error(stackelberg(c(10, 12), c(.2, .18), c(.2, .2),
                           ownerPre = c("A", "B"), leadersPre = "A",
                           normIndex = 1), "normIndex=NA")
  expect_error(stackelberg(c(10, 12), c(.2, .18), c(.2, .2),
                           ownerPre = diag(2), leadersPre = "A"),
               "partial-control matrices")
  ces <- stackelberg(c(10, 12), c(.2, .18), c(.4, .4),
                     ownerPre = c("A", "B"), leadersPre = "A",
                     demand = "ces", gamma = 2, priceOutside = 2,
                     insideSize = 100)
  expect_s4_class(ces, "StackelbergCES")
  expect_error(stackelberg(c(10, 12), c(.2, .18), c(.2, .2),
                           ownerPre = c("A", "B"), leadersPre = "A",
                           demand = "ces", gamma = 2, output = FALSE,
                           priceOutside = 2), "output markets only")
})
