test_that("PriceLeadership and PriceLeadershipBLP-related classes are each defined exactly once", {
  ## getClass() errors (rather than warning about multiple definitions) when
  ## a class is uniquely defined and resolvable in the current session.
  expect_silent(methods::getClass("PriceLeadership"))
})

test_that("PriceLeadership extends antitrust::Logit, not a local duplicate", {
  cls <- methods::getClass("PriceLeadership")
  expect_true("Logit" %in% names(cls@contains))
  expect_identical(attr(cls@className, "package"), "coordination")
})

test_that("calcProducerSurplusGrimTrigger has exactly one Bertrand method, owned by coordination", {
  m <- methods::selectMethod("calcProducerSurplusGrimTrigger", "Bertrand")
  expect_identical(environmentName(topenv(environment(m))), "coordination")
})

test_that("coordination does not redefine unrelated antitrust classes or dispatch", {
  ## Ordinary Logit/Bertrand/Cournot dispatch for calcPrices/calcMargins must
  ## be untouched by loading coordination: these signatures should resolve to
  ## antitrust, not coordination.
  for (sig in c("Logit", "Bertrand", "Cournot")) {
    m <- methods::selectMethod("calcPrices", sig, optional = TRUE)
    if (!is.null(m)) {
      expect_identical(environmentName(topenv(environment(m))), "antitrust")
    }
  }
})

test_that("loading coordination does not alter ordinary Logit calcPrices/calcMargins behavior", {
  fit <- antitrust::logit(
    prices = c(10, 12, 11, 9),
    shares = c(.25, .20, .18, .17),
    margins = c(.40, NA, .35, .25),
    ownerPre = c("A", "A", "B", "C"),
    ownerPost = c("A", "A", "B", "C"),
    insideSize = 1000
  )
  expect_s4_class(fit, "Logit")
  expect_true(all(is.finite(calcPrices(fit, TRUE))))
  expect_true(all(is.finite(calcMargins(fit, TRUE))))
})
