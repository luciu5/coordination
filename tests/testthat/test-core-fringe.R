test_that("exact core-fringe Logit covers hard roles and boundary k", {
  p <- c(10, 11, 12, 13, 14); s <- c(.12, .11, .10, .09, .08)
  owner <- c("A", "A", "B", "C", "C")
  for (conduct in c("bertrand", "cournot")) for (core in list(character(), "A", unique(owner))) {
    fit <- core_fringe(p, s, rep(NA_real_, 5), ownerPre = owner, corePre = core,
                       conduct = conduct, alpha = 1.4, insideSize = 100,
                       priceOutside = .2, control.equ = list())
    expect_s4_class(fit, "CoreFringeLogit")
    expect_equal(unname(fit@pricePre), p, tolerance = 1e-7)
    expect_lt(fit@diagnostics$baselineReproductionRelative, 2e-7)
    expect_lt(core_fringe_residuals(fit)$max, 2e-7)
    expect_equal(unname(calcMargins(fit)), unname(fit@diagnostics$impliedMargins), tolerance = 1e-8)
  }
})

test_that("exact core-fringe CES covers hard roles and missing margins", {
  p <- c(10, 11, 12, 13, 14); s <- c(.12, .11, .10, .09, .08)
  owner <- c("A", "A", "B", "C", "C")
  for (conduct in c("bertrand", "cournot")) for (core in list(character(), "A", unique(owner))) {
    fit <- core_fringe(p, s, rep(NA_real_, 5), ownerPre = owner, corePre = core,
                       demand = "ces", conduct = conduct, gamma = 2,
                       insideSize = 100, priceOutside = 2,
                       control.equ = list())
    expect_s4_class(fit, "CoreFringeCES")
    expect_equal(unname(fit@pricePre), p, tolerance = 1e-7)
    expect_lt(core_fringe_residuals(fit)$max, 2e-7)
  }
})

test_that("core-fringe calibrates partial margins and keeps structural costs", {
  p <- c(10, 11, 12, 13); s <- c(.20, .18, .16, .14); owner <- c("A", "A", "B", "C")
  truth <- core_fringe(p, s, rep(NA_real_, 4), ownerPre = owner, corePre = "A",
                       alpha = 2, insideSize = 100, control.equ = list())
  obs <- truth@diagnostics$impliedMargins; obs[2] <- NA_real_
  fit <- core_fringe(p, s, obs, ownerPre = owner, corePre = "A",
                     insideSize = 100, weights = c(1, 4, 2, 1),
                     control.equ = list())
  expect_equal(fit@diagnostics$demandparam, 2, tolerance = 1e-8)
  expect_equal(fit@diagnostics$observedMargins, obs)
  expect_true(all(is.finite(fit@mcPre)))
})

test_that("ownership, cost, combined shocks and exits preserve exact roles", {
  fit <- core_fringe(c(10, 11, 12, 13), c(.20, .18, .16, .14), rep(NA_real_, 4),
                     ownerPre = c("A", "B", "C", "D"), corePre = "A",
                     alpha = 1.7, insideSize = 100, control.equ = list())
  out <- core_fringe_simulate(fit, ownerPost = c("AB", "AB", "C", "D"), corePost = "AB",
                              mcDelta = c(.05, 0, 0, 0), subset = c(TRUE, TRUE, FALSE, TRUE))
  expect_equal(out@corePost, "AB")
  expect_true(is.na(out@pricePost[3]))
  expect_lt(out@diagnostics$counterfactual$max, 2e-7)
  expect_equal(out@mcPost, fit@mcPre * c(1.05, 1, 1, 1))
  expect_error(core_fringe_simulate(fit, ownerPost = c("AB", "AB", "C", "D")), "corePost")
})
