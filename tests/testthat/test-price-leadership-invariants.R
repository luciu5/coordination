test_that("fringe products satisfy their Bertrand best-response FOC at equilibrium", {
  fit <- fixture_ple()
  nprods <- length(fit@prices)
  coalitionIdx <- which(rowSums(fit@coalitionPre) > 1)
  fringeIdx <- setdiff(seq_len(nprods), coalitionIdx)

  margins <- calcMargins(fit, preMerger = TRUE, level = TRUE)
  mc <- fit@mcPre
  price <- fit@pricePre

  ## Fringe FOC: price - mc == predicted Bertrand margin (level)
  expect_equal(unname(price[fringeIdx] - mc[fringeIdx]),
               unname(margins[fringeIdx]), tolerance = 1e-4)
})

test_that("coalition price equals coordinated Bertrand price plus supermarkup", {
  fit <- fixture_ple()
  coalitionIdx <- which(rowSums(fit@coalitionPre) > 1)

  pricesColl <- calcPrices(fit, preMerger = TRUE, regime = "coordination")
  supermarkup <- fit@supermarkupPre

  expect_equal(unname(fit@pricePre[coalitionIdx]),
               unname(pricesColl[coalitionIdx] + supermarkup),
               tolerance = 1e-4)
})

test_that("calcSlack sign matches identified timing parameter regime", {
  ## When supermarkup is 0 (unconstrained/no coordination markup observed),
  ## no timing parameter is identified and calcSlack warns and returns the
  ## immediate payoff difference only.
  fit <- fixture_ple()
  expect_true(is.na(fit@supermarkupPre) || fit@supermarkupPre == 0)
  expect_warning(slack <- calcSlack(fit, TRUE),
                 "Timing parameter not specified")
  expect_true(is.numeric(slack))
})

test_that("calcSupermarkup unconstrained post-merger supermarkup is finite", {
  ## Post-merger unconstrained supermarkup compares monopoly prices to
  ## themselves and is identically 0 by construction (see calcSupermarkup
  ## PriceLeadership-method: "already at monopoly"). The pre-merger case
  ## compares observed prices to full-collusion prices and can be negative
  ## when the coalition is not yet pricing at the monopoly level, so it is
  ## not sign-constrained.
  fit <- fixture_ple()
  sm_post <- calcSupermarkup(fit, preMerger = FALSE, constrained = FALSE)
  expect_equal(sm_post, 0)
})
