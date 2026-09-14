ces_fixture <- function(gamma = 2, conduct = "bertrand", shares = c(.25, .20, .18, .17),
                        owner = c("A", "B", "C", "D"), leaders = "A", ...) {
  stackelberg(
    prices = c(10, 12, 11, 9, 10)[seq_along(shares)], shares = shares,
    margins = rep(NA_real_, length(shares)), ownerPre = owner,
    leadersPre = leaders, demand = "ces", gamma = gamma, conduct = conduct,
    insideSize = 1000, priceOutside = 2, ...
  )
}

test_that("CES extends antitrust CES with unconditional revenue shares", {
  fit <- ces_fixture(gamma = 2.4)
  expect_s4_class(fit, "StackelbergCES")
  expect_true(methods::is(fit, "CES"))
  expect_equal(fit@slopes$alpha, 1 / sum(fit@shares) - 1)
  expect_equal(fit@mktSize, 1000 / .8)
  expect_equal(calcShares(fit, TRUE, revenue = TRUE), fit@shares, tolerance = 1e-10)
  expect_equal(sum(antitrust::calcRevenues(fit, TRUE)), 1000, tolerance = 1e-8)
  expect_equal(unname(calcQuantities(fit, TRUE)), fit@mktSize * fit@shares / fit@prices,
               tolerance = 1e-8)
  expect_equal(unname(calcMargins(fit, TRUE)), fit@diagnostics$impliedMargins,
               tolerance = 1e-10)
  expect_lt(stackelberg_residuals(fit)$max, 1e-8)
  expect_equal(unname(fit@pricePre), unname(fit@prices), tolerance = 1e-8)
  fast <- ces_fixture(gamma = 2, control.equ = list(implicitCheck = FALSE))
  expect_equal(fast@diagnostics$implicit$statusPre, "not-run")
  expect_true(all(is.na(fast@diagnostics$implicit$pre)))
})

test_that("CES calibration uses all usable log inverse-margin moments", {
  prices <- c(10, 12, 11, 9)
  shares <- c(.25, .20, .18, .17)
  owner <- c("A", "B", "C", "D")
  truth <- ces_fixture(gamma = 2.25)
  observed <- truth@diagnostics$impliedMargins * c(1.01, .98, 1.02, .99)
  observed[3] <- NA_real_
  fit <- stackelberg(prices, shares, observed, ownerPre = owner, leadersPre = "A",
                     demand = "ces", insideSize = 1000, priceOutside = 2,
                     weights = c(1, 2, 3, 4))
  expect_true(is.finite(fit@diagnostics$gamma))
  expect_equal(fit@diagnostics$observedMargins, observed)
  expect_true(fit@diagnostics$usable[1])
  expect_false(fit@diagnostics$usable[3])
  fixed <- stackelberg(prices, shares, rep(NA_real_, 4), ownerPre = owner,
                       leadersPre = "A", demand = "ces", gamma = 2.25,
                       insideSize = 1000, priceOutside = 2)
  expect_equal(fixed@diagnostics$gamma, 2.25)
  expect_error(stackelberg(prices, shares, rep(NA_real_, 4), ownerPre = owner,
                           leadersPre = "A", demand = "ces", insideSize = 1000,
                           priceOutside = 2), "all margins are missing")
})

test_that("CES Bertrand and Cournot nest endpoint timing cases", {
  for (g in c(1.1, 1.5, 2, 6)) for (conduct in c("bertrand", "cournot")) {
    fit <- ces_fixture(gamma = g, conduct = conduct)
    expect_equal(unname(fit@pricePre), unname(fit@prices), tolerance = 1e-7)
    expect_lt(stackelberg_residuals(fit)$max, 1e-7)
    expect_equal(calcPrices(fit, TRUE, method = "implicit"), fit@pricePre,
                 tolerance = 1e-7)
  }
  no_leaders <- ces_fixture(conduct = "cournot", leaders = character())
  all_leaders <- ces_fixture(conduct = "cournot", leaders = c("A", "B", "C", "D"))
  expect_equal(dim(stackelberg_response(no_leaders)), c(4L, 0L))
  expect_equal(dim(stackelberg_response(all_leaders)), c(0L, 4L))
  expect_lt(stackelberg_residuals(no_leaders)$max, 1e-7)
  expect_lt(stackelberg_residuals(all_leaders)$max, 1e-7)
})

test_that("CES Cournot allows a negative analytic response coefficient", {
  fit <- ces_fixture(gamma = 2, conduct = "cournot",
                     shares = c(.03, .82, .08, .04))
  analytic <- stackelberg_response(fit)
  expect_gt(analytic[1, 1], 0)
  expect_equal(analytic, stackelberg_response(fit, method = "implicit"), tolerance = 1e-5)
  q0 <- fit@mktSize * fit@shares[1] / fit@prices[1]
  e <- 1e-3
  qp <- stackelberg_followers(fit, q0 + e)$quantities[-1]
  qm <- stackelberg_followers(fit, q0 - e)$quantities[-1]
  expect_equal(as.numeric((qp - qm) / (2 * e)), as.numeric(analytic), tolerance = 1e-5)
})

test_that("CES multiproduct leaders and followers have product-specific responses", {
  fit <- ces_fixture(gamma = 2, conduct = "bertrand",
                     shares = c(.20, .15, .10, .08, .07),
                     owner = c("A", "A", "B", "C", "C"), leaders = c("A", "B"))
  expect_s4_class(fit, "StackelbergCES")
  expect_equal(dim(stackelberg_response(fit)), c(2L, 3L))
  expect_equal(stackelberg_response(fit), stackelberg_response(fit, method = "implicit"), tolerance = 1e-5)
  expect_lt(stackelberg_residuals(fit)$max, 1e-7)
})

test_that("CES direct leader gradient and actual follower optimization agree", {
  fit <- ces_fixture(gamma = 2, conduct = "cournot")
  q0 <- fit@mktSize * fit@shares[1] / fit@prices[1]
  profit <- function(q) {
    ff <- stackelberg_followers(fit, q)
    (ff$prices[1] - fit@mcPre[1]) * ff$quantities[1]
  }
  expect_lt(abs(numDeriv::grad(profit, q0)), 1e-5)
  expect_lt(stackelberg_residuals(fit)$max, 1e-7)
  fitB <- ces_fixture(gamma = 2, conduct = "bertrand")
  pp <- fitB@prices[1]
  profitB <- function(p) {
    ff <- stackelberg_followers(fitB, p)
    (ff$prices[1] - fitB@mcPre[1]) * ff$quantities[1]
  }
  expect_lt(abs(numDeriv::grad(profitB, pp)), 1e-5)
})

test_that("CES ownership, cost, combined shocks and inactive products preserve roles", {
  fit <- ces_fixture(gamma = 2, conduct = "bertrand")
  own <- stackelberg_simulate(fit, ownerPost = c("AB", "AB", "C", "D"),
                              leadersPost = "AB")
  cost <- stackelberg_simulate(fit, mcDelta = c(.05, 0, 0, 0))
  both <- stackelberg_simulate(fit, ownerPost = c("AB", "AB", "C", "D"),
                               leadersPost = "AB", mcDelta = c(.05, 0, 0, 0))
  expect_lt(stackelberg_residuals(own, FALSE)$max, 1e-7)
  expect_lt(stackelberg_residuals(cost, FALSE)$max, 1e-7)
  expect_lt(stackelberg_residuals(both, FALSE)$max, 1e-7)
  expect_equal(cost@mcPre, fit@mcPre)
  expect_equal(cost@mcPost, fit@mcPre * c(1.05, 1, 1, 1))
  out <- stackelberg_simulate(fit, subset = c(TRUE, TRUE, FALSE, TRUE))
  expect_true(is.na(out@pricePost[3]))
  expect_lt(stackelberg_residuals(out, FALSE)$max, 1e-7)
  reentry <- calcPrices(out, FALSE, subset = rep(TRUE, 4))
  reentryImplicit <- calcPrices(out, FALSE, subset = rep(TRUE, 4), method = "implicit")
  expect_equal(unname(reentry), unname(fit@pricePre), tolerance = 1e-7)
  expect_equal(unname(reentryImplicit), unname(fit@pricePre), tolerance = 1e-7)
  expect_error(stackelberg_simulate(fit, mcDelta = c(-1, 0, 0, 0)), "positive")
})

test_that("CES inverse demand recovers finite prices from quantities", {
  for (g in c(1.1, 2, 6)) {
    fit <- ces_fixture(gamma = g)
    d <- coordination:::.ces_demand_from_prices(fit, fit@pricePre, rep(TRUE, 4))
    back <- coordination:::.ces_demand_from_quantities(fit, d$quantities, rep(TRUE, 4))
    expect_equal(unname(back$prices), unname(fit@pricePre), tolerance = 1e-8)
    expect_equal(unname(back$revenue), unname(fit@shares), tolerance = 1e-8)
  }
})

test_that("CES rejects input markets and invalid outside normalization", {
  expect_error(ces_fixture(output = FALSE), "output markets only")
  expect_error(stackelberg(c(10, 12), c(.2, .18), c(.4, .4), ownerPre = c("A", "B"),
                           leadersPre = "A", demand = "ces", gamma = 2,
                           priceOutside = 0), "positive")
  expect_error(stackelberg(c(10, 12), c(.2, .18), c(.4, .4), ownerPre = c("A", "B"),
                           leadersPre = "A", demand = "ces", gamma = 2,
                           normIndex = 1, priceOutside = 2), "normIndex=NA")
})
