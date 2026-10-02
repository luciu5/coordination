test_that("input Logit Bertrand real rates satisfy fringe and leader FOCs", {
  for (game in c("core_fringe", "stackelberg")) {
    args <- list(
      prices = c(1, 1.2, .9, 1.1),
      shares = c(.20, .15, .10, .08),
      ownerPre = c("A", "A", "B", "C"),
      conduct = "bertrand", output = FALSE, alpha = 1,
      insideSize = 100, price_domain = "real",
      control.equ = list(implicitCheck = FALSE))
    args[[if (game == "core_fringe") "corePre" else "leadersPre"]] <- "A"
    constructor <- if (game == "core_fringe") core_fringe else stackelberg
    simulate_game <- if (game == "core_fringe")
      core_fringe_simulate else stackelberg_simulate
    residuals <- if (game == "core_fringe")
      core_fringe_residuals else stackelberg_residuals
    fit <- do.call(constructor, args)
    out <- simulate_game(fit, mcDelta = rep(-.999, 4))
    expect_true(any(out@pricePost < 0))
    expect_lt(residuals(out, FALSE)$max, 2e-7)
    expect_equal(as.numeric(calcMargins(out, FALSE, level = TRUE)),
      unname(calcMC(out, FALSE) - out@pricePost), tolerance = 2e-7)
    expect_true(all(antitrust::calcShares(out, FALSE, revenue = FALSE) > 0))
    if (game == "stackelberg") {
      follower <- stackelberg_followers(
        out, out@pricePost[1:2], preMerger = FALSE)
      expect_equal(unname(follower$prices), unname(out@pricePost),
                   tolerance = 2e-7)
      expect_equal(unname(calcPrices(out, FALSE, method = "implicit")),
                   unname(out@pricePost), tolerance = 2e-7)
      verified <- do.call(constructor,
        utils::modifyList(args, list(mcDelta = rep(-.999, 4),
                                   control.equ = list(implicitCheck = TRUE))))
      expect_identical(verified@diagnostics$implicit$statusPost,
                       "converged")
      expect_lt(verified@diagnostics$implicit$maxDifferencePost, 2e-7)
    }
  }
})

test_that("positive input Logit Bertrand reports a signed-root obstruction", {
  for (game in c("core_fringe", "stackelberg")) {
    args <- list(
      prices = c(1, 1.2, .9, 1.1),
      shares = c(.20, .15, .10, .08),
      ownerPre = c("A", "A", "B", "C"),
      conduct = "bertrand", output = FALSE, alpha = 1,
      insideSize = 100, control.equ = list(implicitCheck = FALSE))
    args[[if (game == "core_fringe") "corePre" else "leadersPre"]] <- "A"
    constructor <- if (game == "core_fringe") core_fringe else stackelberg
    simulate_game <- if (game == "core_fringe")
      core_fringe_simulate else stackelberg_simulate
    fit <- do.call(constructor, args)
    err <- tryCatch(simulate_game(fit, mcDelta = rep(-.999, 4)),
                    error = identity)
    expect_s3_class(err, "coordination_domain_obstruction")
    expect_identical(err$category, "positive_domain_obstruction")
    expect_lt(err$minimum_rate, 0)
  }
})
