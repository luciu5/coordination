test_that("firm-level Grim Trigger IC matches the deviation threshold in both ownership states", {
  fit <- fixture_bertrand(
    ownerPre = c("A", "A", "B", "C"),
    ownerPost = c("A", "A", "B", "B")
  )
  for (pre in c(TRUE, FALSE)) {
    owner <- if (pre) c("A", "A", "B", "C") else c("A", "A", "B", "B")
    names(owner) <- fit@labels
    prices <- if (pre) fit@pricePre else fit@pricePost
    costs <- if (pre) fit@mcPre else fit@mcPost
    utility <- fit@slopes$meanval +
      fit@slopes$alpha * (prices - fit@priceOutside)
    quantity <- fit@mktSize * exp(utility) / (1 + sum(exp(utility)))
    manual_punish <- (prices - costs) * quantity
    for (discount in c(.05, .8)) {
      gt <- calcProducerSurplusGrimTrigger(
        fit, coalition = c(1, 3), discount = rep(discount, 4), preMerger = pre
      )
      involved <- owner[rownames(gt)]
      expect_equal(nrow(gt), if (pre) 3L else 4L)
      # Independently price the punishment payoff from Logit quantities.
      expect_equal(unname(gt$Punish),
        unname(manual_punish[rownames(gt)]), tolerance = 1e-8)
      for (firm in unique(involved)) {
        rows <- which(involved == firm)
        coord <- sum(gt$Coord[rows])
        defect <- sum(gt$Defect[rows])
        punish <- sum(gt$Punish[rows])
        expect_gt(defect - punish, 0)
        # From C/(1-d) >= D + d P/(1-d): d >= (D-C)/(D-P).
        threshold <- (defect - coord) / (defect - punish)
        expected <- discount >= threshold
        expect_identical(unique(gt$IC[rows]), expected)
      }
      if (discount == .8) expect_true(all(gt$IC))
      if (discount == .05) expect_true(any(!gt$IC))
    }
  }
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
