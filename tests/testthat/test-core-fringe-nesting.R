test_that("core-fringe hard endpoints use the audited firm-level equations", {
  p <- c(2, 2.2, 2.4, 2.6)
  s <- c(.20, .15, .10, .08)
  owner <- c("A", "A", "B", "C")
  firm_share <- c(A = .35, B = .10, C = .08)
  product_firm_share <- unname(firm_share[owner])
  s0 <- 1 - sum(s)
  alpha <- 2.5

  moncom <- core_fringe(
    p, s, rep(NA_real_, 4), ownerPre = owner, corePre = character(),
    conduct = "bertrand", alpha = alpha, insideSize = 100
  )
  expect_equal(unname(calcMargins(moncom)), 1 / (alpha * p), tolerance = 1e-10)

  bertrand <- core_fringe(
    p, s, rep(NA_real_, 4), ownerPre = owner, corePre = unique(owner),
    conduct = "bertrand", alpha = alpha, insideSize = 100
  )
  expect_equal(
    unname(calcMargins(bertrand)),
    1 / ((1 - product_firm_share) * alpha * p), tolerance = 1e-10
  )

  cournot <- core_fringe(
    p, s, rep(NA_real_, 4), ownerPre = owner, corePre = unique(owner),
    conduct = "cournot", alpha = alpha, insideSize = 100
  )
  expect_equal(
    unname(calcMargins(cournot)),
    (1 + product_firm_share / s0) / (alpha * p), tolerance = 1e-10
  )

  gamma <- 2.3
  h <- gamma - 1
  H <- 1 + h * s0
  ces_moncom <- core_fringe(
    p, s, rep(NA_real_, 4), ownerPre = owner, corePre = character(),
    demand = "ces", conduct = "bertrand", gamma = gamma,
    insideSize = 100, priceOutside = 2
  )
  expect_equal(unname(calcMargins(ces_moncom)), rep(1 / gamma, 4),
               tolerance = 1e-10)

  ces_bertrand <- core_fringe(
    p, s, rep(NA_real_, 4), ownerPre = owner, corePre = unique(owner),
    demand = "ces", conduct = "bertrand", gamma = gamma,
    insideSize = 100, priceOutside = 2
  )
  expect_equal(
    unname(calcMargins(ces_bertrand)),
    1 / (gamma - h * product_firm_share), tolerance = 1e-10
  )

  ces_cournot <- core_fringe(
    p, s, rep(NA_real_, 4), ownerPre = owner, corePre = unique(owner),
    demand = "ces", conduct = "cournot", gamma = gamma,
    insideSize = 100, priceOutside = 2
  )
  expect_equal(
    unname(calcMargins(ces_cournot)),
    1 / gamma + h * product_firm_share / (gamma * H), tolerance = 1e-10
  )
})

test_that("core-fringe endpoints reproduce antitrust simultaneous and MonCom games", {
  p <- c(2, 2.2, 2.4, 2.6)
  s <- c(.20, .15, .10, .08)
  owner <- c("A", "A", "B", "C")

  for (conduct in c("bertrand", "cournot")) {
    core <- core_fringe(
      p, s, rep(NA_real_, 4), ownerPre = owner, corePre = unique(owner),
      conduct = conduct, alpha = 2.5, insideSize = 100
    )
    ordinary <- suppressWarnings(if (conduct == "bertrand") {
      antitrust::logit(
        p, s, core@diagnostics$impliedMargins,
        ownerPre = owner, ownerPost = owner, insideSize = 100
      )
    } else {
      antitrust::logit.cournot(
        p, s, core@diagnostics$impliedMargins,
        ownerPre = owner, ownerPost = owner, insideSize = 100
      )
    })
    expect_equal(unname(core@mcPre), unname(ordinary@mcPre), tolerance = 1e-5)
    expect_equal(unname(core@pricePre), unname(ordinary@pricePre), tolerance = 1e-8)

    fringe <- core_fringe(
      p, s, rep(NA_real_, 4), ownerPre = owner, corePre = character(),
      conduct = conduct, alpha = 2.5, insideSize = 100
    )
    atomistic <- suppressWarnings(antitrust::moncom.logit(
      p, s, fringe@diagnostics$impliedMargins,
      ownerPre = owner, ownerPost = owner, insideSize = 100
    ))
    expect_equal(unname(fringe@mcPre), unname(atomistic@mcPre), tolerance = 1e-10)
    expect_equal(unname(fringe@pricePre), unname(atomistic@pricePre), tolerance = 1e-8)
  }

  for (conduct in c("bertrand", "cournot")) {
    core <- core_fringe(
      p, s, rep(NA_real_, 4), ownerPre = owner, corePre = unique(owner),
      demand = "ces", conduct = conduct, gamma = 2.3,
      insideSize = 100, priceOutside = 2
    )
    ordinary <- suppressWarnings(if (conduct == "bertrand") {
      antitrust::ces(
        p, s, core@diagnostics$impliedMargins,
        ownerPre = owner, ownerPost = owner, insideSize = 100,
        priceOutside = 2
      )
    } else {
      antitrust::ces.cournot(
        p, s, core@diagnostics$impliedMargins,
        ownerPre = owner, ownerPost = owner, insideSize = 100,
        priceOutside = 2
      )
    })
    expect_equal(unname(core@mcPre), unname(ordinary@mcPre), tolerance = 2e-5)
    expect_equal(unname(core@pricePre), unname(ordinary@pricePre), tolerance = 1e-8)

    fringe <- core_fringe(
      p, s, rep(NA_real_, 4), ownerPre = owner, corePre = character(),
      demand = "ces", conduct = conduct, gamma = 2.3,
      insideSize = 100, priceOutside = 2
    )
    atomistic <- suppressWarnings(antitrust::moncom.ces(
      p, s, fringe@diagnostics$impliedMargins,
      ownerPre = owner, ownerPost = owner, insideSize = 100,
      priceOutside = 2
    ))
    expect_equal(unname(fringe@mcPre), unname(atomistic@mcPre), tolerance = 1e-10)
    expect_equal(unname(fringe@pricePre), unname(atomistic@pricePre), tolerance = 1e-8)
  }
})
