retention_test_quantities <- function(object, prices) {
  if (methods::is(object, "CES")) {
    g <- object@slopes$gamma
    z <- object@slopes$meanval * prices^(1 - g)
    object@mktSize * z / (object@priceOutside^(1 - g) + sum(z)) / prices
  } else {
    u <- object@slopes$meanval + object@slopes$alpha * (prices - object@priceOutside)
    object@mktSize * exp(u) / (1 + sum(exp(u)))
  }
}

retention_test_prices <- function(object, quantities) {
  if (methods::is(object, "CES")) {
    g <- object@slopes$gamma; A <- object@slopes$meanval
    v <- (object@mktSize * A / quantities)^(1 / g)
    H <- sum(A * v^(1 - g)); outside <- object@priceOutside^(1 - g)
    f <- function(z) z^(g - 1) * (z - H) - outside
    hi <- max(2 * H, 1)
    while (f(hi) < 0) hi <- 2 * hi
    v / stats::uniroot(f, c(H, hi), tol = 1e-12)$root
  } else {
    object@priceOutside +
      (log(quantities / (object@mktSize - sum(quantities))) - object@slopes$meanval) / object@slopes$alpha
  }
}

retention_test_fixture <- function(game, demand, conduct, retention = rep(1, 4), ...) {
  args <- list(prices = c(10, 12, 11, 9), shares = c(.20, .15, .18, .12),
    ownerPre = c("A", "A", "B", "B"), demand = demand, conduct = conduct,
    insideSize = 1, revenueRetentionPre = retention,
    control.equ = list(implicitCheck = FALSE))
  args[[if (demand == "logit") "alpha" else "gamma"]] <- if (demand == "logit") 2 else 3
  args[[if (game == "stackelberg") "leadersPre" else "corePre"]] <- "A"
  do.call(if (game == "stackelberg") stackelberg else core_fringe, c(args, list(...)))
}

test_that("mixed retention satisfies independent portfolio profit conditions", {
  for (game in c("stackelberg", "core")) for (demand in c("logit", "ces")) {
    for (conduct in c("bertrand", "cournot")) {
      base <- retention_test_fixture(game, demand, conduct)
      retained <- c(1, .6, .8, 1)
      simulate <- if (game == "stackelberg") stackelberg_simulate else core_fringe_simulate
      fit <- simulate(base, mcDelta = c(0, .2, .1, 0), revenueRetentionPost = retained)
      p <- fit@pricePost; q <- retention_test_quantities(fit, p)
      action <- if (conduct == "bertrand") p else q
      profit <- function(a, firm) {
        pp <- if (conduct == "bertrand") a else retention_test_prices(fit, a)
        qq <- if (conduct == "bertrand") retention_test_quantities(fit, pp) else a
        own <- fit@firmOwnerPost == firm
        sum(retained[own] * (pp[own] - fit@mcPost[own]) * qq[own])
      }
      expect_equal(fit@ownerPost, base@ownerPost)
      expect_gt(max(abs(p[c(1, 4)] - base@pricePre[c(1, 4)])), 1e-6)
      if (game == "stackelberg") {
        follower_gradient <- numDeriv::grad(function(a) profit(a, "B"), action)[3:4]
        expect_lt(max(abs(follower_gradient)), 2e-7)
        reduced_profit <- function(leader) {
          follower <- stackelberg_followers(fit, leader, preMerger = FALSE)
          profit(if (conduct == "bertrand") follower$prices else follower$quantities, "A")
        }
        leader_gradient <- numDeriv::grad(reduced_profit, action[1:2], method.args = list(eps = 1e-4))
        expect_lt(max(abs(leader_gradient)), 2e-6)
        base_profit <- reduced_profit(action[1:2])
        for (j in 1:2) for (step in c(-1, 1) * 1e-3) {
          alternative <- action[1:2]; alternative[j] <- alternative[j] * (1 + step)
          expect_lte(reduced_profit(alternative), base_profit + 1e-8)
        }
      } else {
        core_gradient <- numDeriv::grad(function(a) profit(a, "A"), action)[1:2]
        expect_lt(max(abs(core_gradient)), 2e-7)
        expected <- if (demand == "logit") rep(1 / abs(fit@slopes$alpha), 2) else p[3:4] / fit@slopes$gamma
        expect_equal(unname(p[3:4] - fit@mcPost[3:4]), unname(expected), tolerance = 1e-8)
      }
    }
  }
})

test_that("mixed retention baseline calibrates and preserves observed equilibrium", {
  for (game in c("stackelberg", "core")) for (demand in c("logit", "ces")) {
    for (conduct in c("bertrand", "cournot")) {
      fit <- retention_test_fixture(game, demand, conduct, c(1, .6, .8, 1))
      expect_equal(unname(fit@pricePre), fit@prices, tolerance = 1e-7)
      expect_equal(unname(fit@pricePost), fit@prices, tolerance = 1e-7)
      diagnostic <- if (game == "stackelberg") stackelberg_residuals(fit) else core_fringe_residuals(fit)
      expect_lt(diagnostic$max, 1e-6)
    }
  }
})

test_that("firm-uniform retention cancels from strategic choices", {
  for (game in c("stackelberg", "core")) {
    fit <- retention_test_fixture(game, "logit", "bertrand")
    simulate <- if (game == "stackelberg") stackelberg_simulate else core_fringe_simulate
    retained <- simulate(fit, revenueRetentionPost = c(.3, .3, .8, .8))
    expect_equal(retained@pricePost, fit@pricePost, tolerance = 1e-8)
    expect_equal(antitrust::getRetention(retained, FALSE), c(.3, .3, .8, .8))
  }
})

test_that("retention-aware calibration recovers demand parameters", {
  for (game in c("stackelberg", "core")) for (demand in c("logit", "ces")) {
    args <- list(prices = c(10, 12, 11, 9), shares = c(.20, .15, .18, .12),
      ownerPre = c("A", "A", "B", "B"), demand = demand, conduct = "bertrand",
      revenueRetentionPre = c(1, .6, .8, 1), control.equ = list(implicitCheck = FALSE))
    args[[if (game == "stackelberg") "leadersPre" else "corePre"]] <- "A"
    parameter <- if (demand == "logit") "alpha" else "gamma"
    args[[parameter]] <- if (demand == "logit") 2 else 3
    fun <- if (game == "stackelberg") stackelberg else core_fringe
    truth <- do.call(fun, args)
    args$margins <- truth@diagnostics$impliedMargins
    args[[parameter]] <- NULL
    fit <- do.call(fun, args)
    expect_equal(fit@diagnostics$demandparam, truth@diagnostics$demandparam,
      tolerance = 1e-6)
  }
})

test_that("mixed-retention simulation handles ownership changes and product exit", {
  for (game in c("stackelberg", "core")) {
    fit <- retention_test_fixture(game, "logit", "bertrand")
    args <- list(object = fit, ownerPost = c("A", "A", "A", "B"),
      subset = c(TRUE, TRUE, TRUE, FALSE), revenueRetentionPost = c(1, .7, .5, 1))
    args[[if (game == "stackelberg") "leadersPost" else "corePost"]] <- "A"
    fun <- if (game == "stackelberg") stackelberg_simulate else core_fringe_simulate
    out <- do.call(fun, args)
    expect_true(all(is.finite(out@pricePost[1:3])))
    expect_true(is.na(out@pricePost[4]))
    expect_equal(out@ownerPost, .sk_owner_matrix(args$ownerPost))
  }
})

test_that("high tariffs approach the domestic-only Logit equilibrium", {
  for (game in c("stackelberg", "core")) for (conduct in c("bertrand", "cournot")) {
    fit <- retention_test_fixture(game, "logit", conduct)
    fun <- if (game == "stackelberg") stackelberg_simulate else core_fringe_simulate
    tau <- c(0, .8, .7, 0)
    high <- fun(fit, mcDelta = tau / (1 - tau), revenueRetentionPost = 1 - tau)
    domestic <- fun(fit, subset = c(TRUE, FALSE, FALSE, TRUE))
    expect_equal(high@pricePost[c(1, 4)], domestic@pricePost[c(1, 4)], tolerance = 1e-7)
    expect_lt(sum(retention_test_quantities(high, high@pricePost)[2:3]), 1e-15)
    expect_true(all(is.finite(high@pricePost)))
  }
})

test_that("mixed-retention routing depends on ratios rather than scale", {
  for (game in c("stackelberg", "core")) {
    reference <- retention_test_fixture(game, "logit", "bertrand", c(1, .6, .8, 1))
    for (scale in c(1e-14, 1e14)) {
      uniform <- antitrust::setRetention(reference,
        retentionPre = scale * c(1, 1, .8, .8))
      expect_false(.coord_mixed_retention(uniform, TRUE))
      fit <- retention_test_fixture(game, "logit", "bertrand",
        scale * c(1, .6, .8, 1))
      expect_true(.coord_mixed_retention(fit, TRUE))
      # A common rescaling of every seller payoff must preserve the weighted
      # baseline calibration. Falling through to unweighted FOCs does not.
      expect_equal(fit@mcPre, reference@mcPre, tolerance = 1e-9)
      expect_equal(calcMargins(fit), calcMargins(reference), tolerance = 1e-9)
      expect_equal(fit@pricePre, reference@pricePre, tolerance = 1e-9)
    }
    tiny <- antitrust::setRetention(reference,
      retentionPre = c(1e-14, 1e-13, 1, 1))
    expect_true(.coord_mixed_retention(tiny, TRUE))
  }
})
