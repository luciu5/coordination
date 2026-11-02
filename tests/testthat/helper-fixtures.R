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

fixture_ple_blp <- function(...) {
  ## Beer-industry style BLP price leadership fixture. Uses a DETERMINISTIC
  ## integration rule (gauss-hermite) plus a supplied meanval so the fit is
  ## RNG-free and reproducible across processes (needed for cross-package
  ## parity). Products 1-3 coordinate; product 4 is fringe.
  shares <- c(.35, .25, .25, .15)
  prices <- c(.93, .88, 1.10, 1.02)
  alpha <- -5.767013
  args <- list(
    prices = prices,
    shares = shares,
    ownerPre = c("Bank1", "Bank2", "Bank3", "Fringe"),
    ownerPost = c("Bank1", "Bank2", "Bank3", "Fringe"),
    coalitionPre = 1:3,
    coalitionPost = 1:3,
    insideSize = 1000,
    integration = "gauss-hermite",
    slopes = list(
      alphaMean = alpha, alpha = alpha, sigma = .5,
      meanval = c(0, log(shares[-1] / shares[1]) - alpha * (prices[-1] - prices[1])),
      sigmaNest = 1
    )
  )
  args[names(list(...))] <- list(...)
  do.call(ple.blp, args)
}
