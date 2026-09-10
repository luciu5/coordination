test_that("ple() constructs a PriceLeadership object with finite equilibria", {
  fit <- fixture_ple()
  expect_s4_class(fit, "PriceLeadership")
  expect_true(all(is.finite(fit@pricePre)))
  expect_true(all(is.finite(fit@pricePost)))
  expect_true(all(is.finite(calcMargins(fit, TRUE))))
  expect_true(all(is.finite(calcShares(fit, TRUE))))
})

test_that("ple() matches known parity fixture values", {
  fit <- fixture_ple()
  expect_equal(unname(fit@slopes$alpha), -0.5354752, tolerance = 1e-6)
  expect_equal(unname(fit@supermarkupPre), 0, tolerance = 1e-6)
})

test_that("calcSlack, calcSupermarkup, calcPriceLeadershipParams are exported and usable", {
  fit <- fixture_ple()
  expect_true(is.function(calcSlack))
  expect_true("calcSupermarkup" %in% getNamespaceExports("coordination"))
  expect_true("calcPriceLeadershipParams" %in% getNamespaceExports("coordination"))
  expect_true(is.function(calcSupermarkup))
  expect_true(is.function(calcPriceLeadershipParams))
  expect_warning(slack <- calcSlack(fit, TRUE),
                  "Timing parameter not specified")
  expect_true(all(is.finite(slack)))
  sm <- calcSupermarkup(fit, FALSE, constrained = FALSE)
  expect_true(is.finite(sm))
})

test_that("PriceLeadershipBLP / ple.blp are present and exported", {
  expect_true(exists("ple.blp", where = asNamespace("coordination"), inherits = FALSE))
  expect_true("ple.blp" %in% getNamespaceExports("coordination"))
  expect_true(isVirtualClass("PriceLeadershipBLP") || methods::existsMethod("calcSlopes", "PriceLeadershipBLP"))
  expect_true("PriceLeadershipBLP" %in% methods::getClasses(asNamespace("coordination")))
})

test_that("ple.blp() constructs a PriceLeadershipBLP with finite equilibria", {
  fit <- fixture_ple_blp()
  expect_s4_class(fit, "PriceLeadershipBLP")
  expect_identical(fit@slopes$integration, "gauss-hermite")
  expect_true(all(is.finite(fit@pricePre)))
  expect_true(all(is.finite(fit@pricePost)))
  expect_true(all(is.finite(calcShares(fit, TRUE))))
})
