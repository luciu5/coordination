# BLP-demand price leadership regressions. Ported from antitrust's
# test-blp-integration-regressions.R (the two PriceLeadershipBLP tests that
# were removed when PLE-BLP migrated here). The demographic-node test is
# rewritten to avoid antitrust private access: instead of recomputing the
# Gauss-Hermite nodes via antitrust:::.blp_normal_nodes, we assert the
# observable public-slot identity demogDraws == demogMean + sqrt(demogCov) *
# consDraws, which holds for the one-demographic, sigma = 0 GH case.

test_that("PriceLeadershipBLP uses the shared integration rules", {
  shares <- c(.35, .25, .25, .15)
  prices <- c(.93, .88, 1.10, 1.02)
  alpha <- -5.767013
  common <- list(
    prices = prices, shares = shares,
    ownerPre = c("Bank1", "Bank2", "Bank3", "Fringe"),
    ownerPost = c("Bank1", "Bank2", "Bank3", "Fringe"),
    coalitionPre = 1:3, coalitionPost = 1:3,
    insideSize = 1000,
    slopes = list(
      alphaMean = alpha, alpha = alpha, sigma = .5,
      meanval = c(0, log(shares[-1] / shares[1]) -
        alpha * (prices[-1] - prices[1])), sigmaNest = 1
    )
  )
  default_rule <- suppressWarnings(do.call(ple.blp, common))
  expect_identical(default_rule@slopes$integration, "gauss-hermite")
  expect_length(default_rule@slopes$consDraws, 31L)

  quadrature <- suppressWarnings(do.call(
    ple.blp, c(common, list(integration = "gauss-hermite", nNodes = 15L))
  ))
  expect_identical(quadrature@slopes$integration, "gauss-hermite")
  expect_length(quadrature@slopes$consDraws, 15L)
  expect_equal(sum(quadrature@slopes$drawWeights), 1, tolerance = 1e-14)

  monte_carlo <- suppressWarnings(do.call(
    ple.blp, c(common, list(integration = "monte-carlo", nDraws = 15L))
  ))
  expect_identical(monte_carlo@slopes$integration, "monte-carlo")
  expect_length(monte_carlo@slopes$consDraws, 15L)
  expect_equal(monte_carlo@slopes$drawWeights, rep(1 / 15, 15),
               tolerance = 0)
})


test_that("PriceLeadershipBLP uses shared Gauss-Hermite nodes for one demographic", {
  shares <- c(.35, .25, .25, .15)
  prices <- c(.93, .88, 1.10, 1.02)
  common <- list(
    prices = prices, shares = shares,
    ownerPre = c("Bank1", "Bank2", "Bank3", "Fringe"),
    ownerPost = c("Bank1", "Bank2", "Bank3", "Fringe"),
    coalitionPre = 1:3, coalitionPost = 1:3,
    insideSize = 1000,
    slopes = list(
      alphaMean = -5.767013, alpha = -5.767013, sigma = 0,
      piDemog = .4, demogMean = .2,
      demogCov = matrix(4, nrow = 1L, ncol = 1L),
      meanval = c(0, log(shares[-1] / shares[1]) -
        (-5.767013) * (prices[-1] - prices[1])), sigmaNest = 1
    )
  )
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(20260907)
  before <- .Random.seed
  fit <- suppressWarnings(do.call(ple.blp, common))
  after <- .Random.seed

  ## Deterministic Gauss-Hermite path consumes no RNG.
  expect_identical(before, after)
  expect_identical(fit@slopes$nDemog, 1L)
  expect_identical(fit@slopes$integration, "gauss-hermite")

  ## Public-slot identity for the one-demographic, sigma = 0 GH case:
  ## demogDraws = demogMean + sqrt(demogCov) * consDraws. This pins the
  ## shared-node behavior without touching antitrust internals.
  expect_equal(fit@slopes$demogDraws,
               matrix(.2 + 2 * fit@slopes$consDraws, ncol = 1L),
               tolerance = 0)
})


test_that("PriceLeadershipBLP preserves canonical two-dimensional integration state", {
  shares <- c(.35, .25, .25, .15)
  prices <- c(.93, .88, 1.10, 1.02)
  common <- list(
    prices = prices, shares = shares,
    ownerPre = c("Bank1", "Bank2", "Bank3", "Fringe"),
    ownerPost = c("Bank1", "Bank2", "Bank3", "Fringe"),
    coalitionPre = 1:3, coalitionPost = 1:3,
    insideSize = 1000,
    slopes = list(
      alphaMean = -5.767013, alpha = -5.767013, sigma = .5,
      nDemog = 1L, piDemog = .4, demogMean = .2,
      demogCov = matrix(4, nrow = 1L, ncol = 1L),
      sigmaNest = 1
    )
  )

  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(20260910)
  before <- .Random.seed
  fit <- suppressMessages(suppressWarnings(do.call(
    ple.blp, c(common, list(integration = "gauss-hermite", nNodes = c(3L, 4L)))
  )))
  after <- .Random.seed

  expect_identical(before, after)
  expect_identical(fit@slopes$integration, "gauss-hermite")
  expect_identical(fit@slopes$factorOrder, c("price", "demog1"))
  expect_identical(fit@slopes$nodesPerAxis, c(3L, 4L))
  expect_identical(fit@slopes$nNodes, c(3L, 4L))
  expect_identical(fit@slopes$nDraws, 12L)
  expect_identical(fit@nDraws, 12)
  expect_true(is.matrix(fit@slopes$integrationPoints))
  expect_equal(dim(fit@slopes$integrationPoints), c(12L, 2L))
  expect_true(is.numeric(fit@slopes$consDraws))
  expect_false(is.matrix(fit@slopes$consDraws))
  expect_equal(sum(fit@slopes$drawWeights), 1, tolerance = 1e-14)
  observed_state <- fit
  observed_state@pricePre <- prices
  expect_equal(unname(calcShares(observed_state, TRUE)), shares, tolerance = 1e-10)
  expect_true(all(is.finite(fit@pricePre)))
  expect_true(all(is.finite(fit@pricePost)))
  expect_true(all(is.finite(calcShares(fit, TRUE))))

  coalition <- which(rowSums(fit@coalitionPre) > 1)
  fringe <- setdiff(seq_along(fit@prices), coalition)
  expect_length(fringe, 1L)
  fringe_price <- fit@pricePre[fringe]
  fringe_mc <- fit@mcPre[fringe]
  profit_at_price <- function(price) {
    perturbed <- fit
    perturbed@pricePre[fringe] <- price
    fringe_share <- calcShares(perturbed, preMerger = TRUE)[fringe]
    fringe_quantity <- perturbed@mktSize * fringe_share
    (price - fringe_mc) * fringe_quantity
  }
  step <- 1e-5 * max(1, abs(fringe_price))
  fringe_profit_derivative <- (
    profit_at_price(fringe_price + step) -
      profit_at_price(fringe_price - step)
  ) / (2 * step)

  ## The fitted fringe price is a Bertrand optimum.  A 1e-5 price step keeps
  ## central-difference truncation below the 1e-3 profit-per-price tolerance
  ## while allowing ordinary floating-point evaluation error.
  expect_lt(abs(fringe_profit_derivative), 1e-3)

  reused_args <- common
  reused_args$slopes <- fit@slopes
  set.seed(20260911)
  reuse_before <- .Random.seed
  reused <- suppressMessages(suppressWarnings(do.call(ple.blp, reused_args)))
  reuse_after <- .Random.seed
  expect_identical(reuse_before, reuse_after)
  expect_identical(reused@slopes$integration, fit@slopes$integration)
  expect_identical(reused@slopes$integrationPoints,
                   fit@slopes$integrationPoints)
  expect_equal(reused@slopes$drawWeights, fit@slopes$drawWeights,
               tolerance = 1e-14)
  expect_identical(reused@slopes$factorOrder, fit@slopes$factorOrder)
  expect_identical(reused@slopes$nodesPerAxis, fit@slopes$nodesPerAxis)
  expect_identical(reused@slopes$nNodes, fit@slopes$nNodes)
  expect_equal(reused@pricePre, fit@pricePre, tolerance = 1e-10)
  expect_equal(reused@pricePost, fit@pricePost, tolerance = 1e-10)
  expect_equal(calcShares(reused, TRUE), calcShares(fit, TRUE), tolerance = 1e-12)
  expect_equal(calcShares(reused, FALSE), calcShares(fit, FALSE), tolerance = 1e-12)
})


test_that("PriceLeadershipBLP accepts two-dimensional supplied points and seeded Monte Carlo", {
  shares <- c(.35, .25, .25, .15)
  prices <- c(.93, .88, 1.10, 1.02)
  common <- list(
    prices = prices, shares = shares,
    ownerPre = c("Bank1", "Bank2", "Bank3", "Fringe"),
    ownerPost = c("Bank1", "Bank2", "Bank3", "Fringe"),
    coalitionPre = 1:3, coalitionPost = 1:3,
    insideSize = 1000,
    slopes = list(
      alphaMean = -5.767013, alpha = -5.767013, sigma = .5,
      nDemog = 1L, piDemog = .4, demogMean = .2,
      demogCov = matrix(4, nrow = 1L, ncol = 1L),
      meanval = c(0, log(shares[-1] / shares[1]) -
        (-5.767013) * (prices[-1] - prices[1])), sigmaNest = 1
    )
  )
  points <- matrix(c(-1, -1, -1, 1, 1, -1, 1, 1), ncol = 2L, byrow = TRUE)
  weights <- c(1, 2, 3, 4)
  supplied <- suppressMessages(suppressWarnings(do.call(
    ple.blp, c(common, list(
      integration = "provided", integrationPoints = points,
      integrationWeights = weights
    ))
  )))

  expect_identical(supplied@slopes$integration, "provided")
  expect_equal(supplied@slopes$integrationPoints, points, tolerance = 0)
  expect_equal(supplied@slopes$drawWeights, weights / sum(weights), tolerance = 0)
  expect_equal(supplied@slopes$integrationWeights,
               weights / sum(weights), tolerance = 0)
  expect_null(supplied@slopes$nNodes)
  expect_identical(supplied@nDraws, 4)

  set.seed(20260910)
  mc1 <- suppressMessages(suppressWarnings(do.call(
    ple.blp, c(common, list(integration = "monte-carlo", nDraws = 7L))
  )))
  set.seed(20260910)
  mc2 <- suppressMessages(suppressWarnings(do.call(
    ple.blp, c(common, list(integration = "monte-carlo", nDraws = 7L))
  )))
  expect_identical(mc1@slopes$integration, "monte-carlo")
  expect_identical(mc1@slopes$nDraws, 7L)
  expect_true(is.matrix(mc1@slopes$integrationPoints))
  expect_identical(dim(mc1@slopes$integrationPoints), c(7L, 2L))
  expect_equal(mc1@slopes$drawWeights, rep(1 / 7, 7), tolerance = 0)
  expect_identical(mc1@slopes$integrationPoints, mc2@slopes$integrationPoints)
  expect_identical(mc1@pricePre, mc2@pricePre)
  expect_identical(mc1@pricePost, mc2@pricePost)
})


test_that("ple.blp with a fringe firm and no supplied margins recovers finite outputs", {
  fit <- fixture_ple_blp()
  expect_s4_class(fit, "PriceLeadershipBLP")
  expect_true(all(is.na(fit@margins)))
  expect_true(all(is.finite(fit@pricePre)))
  expect_true(all(is.finite(fit@pricePost)))
  expect_true(is.finite(fit@supermarkupPost))
})
