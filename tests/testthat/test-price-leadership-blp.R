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


test_that("ple.blp with a fringe firm and no supplied margins recovers finite outputs", {
  fit <- fixture_ple_blp()
  expect_s4_class(fit, "PriceLeadershipBLP")
  expect_true(all(is.na(fit@margins)))
  expect_true(all(is.finite(fit@pricePre)))
  expect_true(all(is.finite(fit@pricePost)))
  expect_true(is.finite(fit@supermarkupPost))
})
