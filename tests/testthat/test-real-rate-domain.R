test_that("the shared log-domain scalar is finite over ordinary and extreme logs", {
  logs <- c(-.9, -.5, 0, .5, 1, 10, 1000)
  got <- vapply(logs, coordination:::.coordination_log_w_exp, numeric(1))
  expect_true(all(is.finite(got) & got > 0))
  expect_lt(max(abs(log(got) + got - logs)), 2e-10)
})

test_that("real CoreFringe input Cournot preserves roles, shares, and dollar markdowns", {
  fit <- core_fringe(
    prices = c(1, 1.2, .9), shares = c(.20, .15, .10),
    ownerPre = c("A", "A", "B"), corePre = "A",
    conduct = "cournot", output = FALSE, alpha = 1,
    insideSize = 100, price_domain = "real"
  )
  expect_identical(fit@control.equ$price_domain, "real")
  expect_identical(fit@diagnostics$price_domain, "real")
  out <- core_fringe_simulate(fit, mcDelta = rep(-.999, 3))
  active <- out@subset
  expect_true(any(out@pricePost[active] <= 0))
  expect_lt(core_fringe_residuals(out, FALSE)$max, 2e-7)
  expect_equal(
    as.numeric(calcMargins(out, FALSE, level = TRUE)[active]),
    unname(calcMC(out, FALSE)[active] - out@pricePost[active]),
    tolerance = 2e-10
  )
  expect_true(all(antitrust::calcShares(out, FALSE, revenue = FALSE)[active] > 0))
  revenue <- antitrust::calcShares(out, FALSE, revenue = TRUE)
  expect_true(all(is.na(revenue)))
  expect_true(isTRUE(attr(revenue, "diagnostics")$revenue_share_undefined[1]))
})

test_that("real Stackelberg input Cournot agrees across analytic and implicit routes", {
  fit <- stackelberg(
    prices = c(1, 1.2, .9, 1.1), shares = c(.20, .15, .10, .08),
    ownerPre = c("A", "A", "B", "C"), leadersPre = "A",
    conduct = "cournot", output = FALSE, alpha = 1,
    insideSize = 100, price_domain = "real",
    control.equ = list(implicitCheck = FALSE)
  )
  out <- stackelberg_simulate(fit, mcDelta = rep(-.999, 4))
  active <- out@subset
  expect_true(any(out@pricePost[active] <= 0))
  expect_lt(stackelberg_residuals(out, FALSE)$max, 2e-7)
  implicit <- calcPrices(out, FALSE, method = "implicit")
  expect_equal(unname(implicit[active]), unname(out@pricePost[active]),
               tolerance = 2e-7)
  expect_equal(
    as.numeric(calcMargins(out, FALSE, level = TRUE)[active]),
    unname(calcMC(out, FALSE)[active] - out@pricePost[active]),
    tolerance = 2e-10
  )
  merged <- stackelberg_simulate(
    fit, ownerPost = c("AB", "AB", "B", "C"), leadersPost = "AB",
    mcDelta = c(-.999, -.999, -.999, -.999)
  )
  expect_lt(stackelberg_residuals(merged, FALSE)$max, 2e-7)
  follower_only <- stackelberg_simulate(
    fit, subset = c(FALSE, FALSE, TRUE, TRUE),
    mcDelta = rep(-.999, 4)
  )
  follower_active <- follower_only@subset
  expect_true(all(follower_only@pricePost[follower_active] < 0))
  follower_implicit <- calcPrices(follower_only, FALSE, method = "implicit")
  expect_equal(unname(follower_implicit[follower_active]),
               unname(follower_only@pricePost[follower_active]),
               tolerance = 2e-7)
  expect_lt(stackelberg_residuals(follower_only, FALSE)$max, 2e-7)
  all_leaders <- stackelberg(
    prices = c(1, 1.2, .9), shares = c(.20, .15, .10),
    ownerPre = c("A", "B", "C"), leadersPre = c("A", "B", "C"),
    conduct = "cournot", output = FALSE, alpha = 1,
    insideSize = 100, price_domain = "real",
    control.equ = list(implicitCheck = FALSE)
  )
  all_leaders_post <- stackelberg_simulate(
    all_leaders, mcDelta = rep(-.999, 3)
  )
  expect_length(stackelberg_residuals(all_leaders_post, FALSE)$follower, 0L)
})

test_that("zero rates keep dollar markdowns and flag undefined normalized output", {
  fit <- core_fringe(
    prices = c(1, 1.2), shares = c(.20, .15),
    ownerPre = c("A", "B"), corePre = "A",
    conduct = "cournot", output = FALSE, alpha = 1,
    insideSize = 100, price_domain = "real"
  )
  ## Move the fringe marginal cost to its exact zero-rate FOC, then recompute
  ## the equilibrium.  The zero is therefore an equilibrium rate, not a
  ## manually overwritten reporting value.
  fit@mcPost[2] <- 1
  fit@pricePost <- calcPrices(fit, FALSE)
  expect_equal(unname(fit@pricePost[2]), 0, tolerance = 2e-12)
  expect_lt(core_fringe_residuals(fit, FALSE)$max, 2e-7)
  normalized <- calcMargins(fit, FALSE)
  dollar <- calcMargins(fit, FALSE, level = TRUE)
  expect_true(is.na(normalized[2]))
  expect_true(is.finite(dollar[2]))
  expect_true(is.finite(as.numeric(antitrust::calcProducerSurplus(fit, FALSE)[2])))
  expect_true(isTRUE(attr(normalized, "diagnostics")$normalized_undefined_zero[2]))
  expect_true(isTRUE(attr(normalized, "diagnostics")$signed_negative_price[2] == FALSE))
})

test_that("zero Stack rates retain finite FOCs and flag undefined normalization", {
  fit <- stackelberg(
    prices = 1, shares = plogis(1), ownerPre = "A", leadersPre = "A",
    conduct = "cournot", output = FALSE, alpha = 1, price_domain = "real",
    control.equ = list(implicitCheck = FALSE)
  )
  ## The fixed utility fixture u(p)=p and post value 2 imply x=W(e)=1,
  ## an exact zero rate, share 1/2, and dollar markdown 2.  No clipping.
  fit@slopes$meanval <- 0
  fit@mcPost <- 2
  fit@pricePost <- calcPrices(fit, FALSE)
  diag <- stackelberg_residuals(fit, FALSE)
  expect_identical(unname(fit@pricePost), 0)
  expect_equal(unname(calcShares(fit, FALSE)), .5, tolerance = 2e-12)
  expect_equal(unname(as.numeric(calcMargins(fit, FALSE, level = TRUE))), 2)
  expect_true(is.na(calcMargins(fit, FALSE)[1]))
  expect_lt(diag$max, 2e-10)
  expect_true(diag$undefined_normalized_zero[1])
  expect_true(is.na(diag$normalizedLeader[1]))
  expect_true(is.na(diag$maxNormalized))
  expect_true(is.finite(as.numeric(antitrust::calcProducerSurplus(fit, FALSE))))
})

test_that("real domain rejects output, Bertrand, and CES combinations", {
  expect_error(
    core_fringe(c(1, 1.2), c(.2, .15), ownerPre = c("A", "B"),
                corePre = "A", output = TRUE, price_domain = "real"),
    "only input Logit Cournot"
  )
  expect_error(
    stackelberg(c(1, 1.2), c(.2, .15), ownerPre = c("A", "B"),
                leadersPre = "A", conduct = "bertrand", output = FALSE,
                alpha = 1, price_domain = "real"),
    "only input Logit Cournot"
  )
  expect_error(
    core_fringe(c(1, 1.2), c(.2, .15), ownerPre = c("A", "B"),
                corePre = "A", demand = "ces", gamma = 2,
                price_domain = "real"),
    "only input Logit Cournot"
  )
})

test_that("positive domain remains numerically unchanged and typed obstruction is available", {
  args <- list(
    prices = c(10, 11, 12), shares = c(.20, .15, .10),
    ownerPre = c("A", "B", "C"), corePre = "A",
    conduct = "cournot", output = FALSE, alpha = 1.5, insideSize = 100
  )
  implicit <- do.call(core_fringe, c(args, list(price_domain = "positive")))
  default <- do.call(core_fringe, args)
  expect_equal(implicit@pricePre, default@pricePre, tolerance = 2e-10)
  expect_equal(as.numeric(antitrust::CV(default)), as.numeric(antitrust::CV(implicit)),
               tolerance = 2e-10)
  expect_identical(default@control.equ$price_domain, "positive")
  err <- tryCatch(
    core_fringe_simulate(default, mcDelta = rep(-.999, 3)),
    error = function(e) e
  )
  expect_s3_class(err, "coordination_domain_obstruction")
  expect_identical(err$category, "positive_domain_obstruction")
  expect_identical(err$domain, "positive")
  expect_false(err$preMerger)
})
