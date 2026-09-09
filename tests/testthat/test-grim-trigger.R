test_that("calcProducerSurplusGrimTrigger returns expected structure", {
  fit <- fixture_bertrand()
  gt <- calcProducerSurplusGrimTrigger(
    fit, coalition = 1:2, discount = c(0.9, 0.9, 0.5, 0.5), preMerger = TRUE
  )
  expect_s3_class(gt, "data.frame")
  expect_true(all(c("Coalition", "Discount", "Coord", "Defect", "Punish", "IC") %in% names(gt)))
})

test_that("IC holds under a high discount factor (patient firms sustain coordination)", {
  fit <- fixture_bertrand()
  gt <- calcProducerSurplusGrimTrigger(
    fit, coalition = 1:2, discount = c(0.95, 0.95, 0.5, 0.5), preMerger = TRUE
  )
  expect_true(all(gt$IC))
})

test_that("IC fails under a very low discount factor (impatient firms defect)", {
  fit <- fixture_bertrand()
  gt <- calcProducerSurplusGrimTrigger(
    fit, coalition = 1:2, discount = c(0.01, 0.01, 0.5, 0.5), preMerger = TRUE
  )
  expect_true(all(!gt$IC))
})

test_that("Coord/Defect/Punish satisfy pi_Coord >= pi_Punish (coordination weakly dominates static Bertrand)", {
  fit <- fixture_bertrand()
  gt <- calcProducerSurplusGrimTrigger(
    fit, coalition = 1:2, discount = c(0.9, 0.9, 0.5, 0.5), preMerger = TRUE
  )
  expect_true(all(gt$Coord >= gt$Punish - 1e-8))
})

test_that("Defect profits are at least as large as Coord profits in the deviation period", {
  ## By construction, one-period optimal deviation cannot do worse than
  ## continuing to coordinate.
  fit <- fixture_bertrand()
  gt <- calcProducerSurplusGrimTrigger(
    fit, coalition = 1:2, discount = c(0.9, 0.9, 0.5, 0.5), preMerger = TRUE
  )
  expect_true(all(gt$Defect >= gt$Coord - 1e-8))
})

test_that("post-merger Grim Trigger analysis runs and returns finite values", {
  fit <- fixture_bertrand()
  gt <- calcProducerSurplusGrimTrigger(
    fit, coalition = 1:2, discount = c(0.9, 0.9, 0.5, 0.5), preMerger = FALSE
  )
  expect_true(all(is.finite(gt$Coord)))
  expect_true(all(is.finite(gt$Defect)))
  expect_true(all(is.finite(gt$Punish)))
})

test_that("isCollusion = TRUE recalibrates demand under the collusive ownership assumption", {
  fit <- fixture_bertrand()
  gt_default <- calcProducerSurplusGrimTrigger(
    fit, coalition = 1:2, discount = c(0.9, 0.9, 0.5, 0.5), preMerger = TRUE,
    isCollusion = FALSE
  )
  gt_collusion <- calcProducerSurplusGrimTrigger(
    fit, coalition = 1:2, discount = c(0.9, 0.9, 0.5, 0.5), preMerger = TRUE,
    isCollusion = TRUE
  )
  expect_s3_class(gt_collusion, "data.frame")
  ## isCollusion changes the calibration basis, so results generally differ.
  expect_false(isTRUE(all.equal(gt_default$Coord, gt_collusion$Coord)))
})

test_that("calcProducerSurplusGrimTrigger validates coalition and discount inputs", {
  fit <- fixture_bertrand()
  expect_error(
    calcProducerSurplusGrimTrigger(fit, coalition = 99, discount = 0.9, preMerger = TRUE),
    "coalition"
  )
  expect_error(
    calcProducerSurplusGrimTrigger(fit, coalition = 1:2, discount = c(1.5, 0.9, NA, NA), preMerger = TRUE),
    "discount"
  )
})
