fixture_ple <- function(...) {
  args <- list(
    prices = c(10, 12, 11, 9),
    shares = c(.25, .20, .18, .17),
    margins = c(.40, NA, .35, .25),
    ownerPre = c("A", "A", "B", "C"),
    ownerPost = c("A", "A", "B", "C"),
    coalitionPre = 1:3,
    insideSize = 1000
  )
  args[names(list(...))] <- list(...)
  do.call(ple, args)
}

fixture_bertrand <- function(...) {
  ## Distinct single-product firms so that a coalition of products 1:2 is a
  ## genuine multi-firm coordination, not already-common ownership.
  args <- list(
    prices = c(10, 12, 11, 9),
    shares = c(.25, .20, .18, .17),
    margins = c(.40, .38, .35, .25),
    ownerPre = c("A", "B", "C", "D"),
    ownerPost = c("A", "B", "C", "D"),
    insideSize = 1000
  )
  args[names(list(...))] <- list(...)
  do.call(antitrust::logit, args)
}
