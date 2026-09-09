# coordination

`coordination` provides coordinated-effects models for merger analysis, built on
the structural demand and cost calibration infrastructure in
[`antitrust`](https://github.com/luciu5/antitrust). While `antitrust` provides
the underlying demand systems and unilateral (non-coordinated) merger simulation
machinery -- Logit, CES, Bertrand, Cournot, BLP, auctions, and bargaining --
`coordination` adds models of coordinated conduct: firms setting prices above
the unilateral competitive level and sustaining that outcome through repeated
interaction.

This package currently implements two coordinated-effects models:

1. **Price Leadership Equilibrium (PLE)** -- a coordinating coalition prices
   above Bertrand levels subject to incentive-compatibility (IC) constraints,
   with a competitive fringe that best-responds.
2. **Grim Trigger sustainability analysis** -- whether a coalition of firms
   playing ordinary Bertrand can sustain collusive pricing in a repeated game.

## Installation

```r
# install.packages("devtools")
devtools::install_github("luciu5/antitrust")
devtools::install_github("luciu5/coordination")
```

## Price Leadership Equilibrium

The price leadership model follows Mansley, Miller, Sheu & Weinberg (2023). A
coordinating coalition of firms announces a *supermarkup* above the Bertrand
price; each coalition firm's incentive to defect from that supermarkup is
checked against a firm-specific *timing/discount parameter*; and a competitive
fringe best-responds to the coalition's prices.

```r
library(antitrust)
library(coordination)

## Beer industry example (Mansley, Miller, Sheu & Weinberg 2023)
fit <- ple(
  prices       = c(0.93, 0.88, 1.10, 1.02),
  shares       = c(0.35, 0.25, 0.25, 0.15),
  margins      = c(0.40, NA,   0.35, 0.25),   # coalition margins + one fringe margin
  ownerPre     = c("AB", "AB", "MC", "Fringe"),
  ownerPost    = c("AB", "AB", "MC", "Fringe"),
  coalitionPre = c(1, 2, 3),                  # products 1-3 coordinate; 4 is fringe
  coalitionPost = c(1, 2, 3),
  insideSize   = 1000
)

summary(fit)

fit@supermarkupPre    # equilibrium supermarkup above Bertrand
fit@timingParam        # calibrated firm-specific timing/discount parameter (NA if no IC binds)
fit@bindingFirm        # which coalition firm has the binding IC constraint (NA if none)

calcSlack(fit)         # incentive-compatibility slack for each coalition firm

## calcSupermarkup(constrained = TRUE) finds the maximum IC-sustainable
## supermarkup once a firm-specific timing parameter has been identified
## from a binding constraint (see fit@bindingFirm above).
calcSupermarkup(fit, constrained = FALSE)  # unconstrained (full-collusion) supermarkup
```

### Model overview

* **Coalition and fringe.** `coalitionPre`/`coalitionPost` identify which
  products participate in coordination; all other products are fringe and
  price at their ordinary Bertrand best response.
* **Supermarkup.** The coalition prices at the coordinated Bertrand price plus
  a markup `m` (`supermarkupPre`/`supermarkupPost`).
* **Incentive compatibility.** A coalition firm's IC constraint compares its
  profit from continuing to coordinate against the one-period gain from
  optimally undercutting the coalition, discounted against the Bertrand
  punishment that follows detection.
* **Timing parameter.** When a firm's IC constraint binds, a firm-specific
  timing/discount parameter is calibrated from the binding constraint.

`PriceLeadership` extends `antitrust`'s `Logit` class, so calibrated
`PriceLeadership` fits support the ordinary `antitrust` generics
(`calcPrices`, `calcMargins`, `calcShares`, `calcProducerSurplus`, ...) in
addition to the coordination-specific ones (`calcSlack`, `calcSupermarkup`,
`calcPriceLeadershipParams`).

## Grim Trigger sustainability analysis

`calcProducerSurplusGrimTrigger` evaluates whether a coalition of firms
playing an ordinary `antitrust` `Bertrand` model can sustain a fully collusive
price under Grim Trigger strategies: each firm cooperates so long as every
other coalition firm cooperated in the prior period, and permanently reverts
to Bertrand pricing (the "punishment") after any defection.

```r
library(antitrust)
library(coordination)

fit <- logit(
  prices    = c(10, 12, 11, 9),
  shares    = c(.25, .20, .18, .17),
  margins   = c(.40, .38, .35, .25),
  ownerPre  = c("A", "B", "C", "D"),
  ownerPost = c("A", "B", "C", "D"),
  insideSize = 1000
)

## Can firms A and B (products 1-2) sustain collusion at discount factor 0.9?
calcProducerSurplusGrimTrigger(
  fit,
  coalition = 1:2,
  discount  = c(0.9, 0.9, 0.5, 0.5),
  preMerger = TRUE
)
```

The returned data frame reports, for each product produced by a coalition
firm, the single-period producer surplus from coordinating (`Coord`),
defecting (`Defect`), and Bertrand punishment (`Punish`), along with whether
the firm's incentive-compatibility constraint holds (`IC`).

`calcProducerSurplusGrimTrigger` is defined as a method on `antitrust`'s
`Bertrand` class -- any `antitrust` model built on `Bertrand` (Logit, CES,
BLP, nested Logit, ...) can be analyzed this way.

## Package boundary

* `antitrust` provides the structural demand/cost calibration core: Logit,
  CES, Bertrand, Cournot, BLP, auctions, and bargaining. It has no dependency
  on `coordination`.
* `coordination` depends on `antitrust` and extends its classes
  (`PriceLeadership` extends `Logit`) and generics
  (`calcProducerSurplusGrimTrigger` is a method on `Bertrand`). It uses only
  `antitrust`'s exported/public API.
* Ordinary Stackelberg and Cournot conduct remain in `antitrust` -- they are
  not coordinated-effects models in the sense used here.

## PLE with BLP demand (`PriceLeadershipBLP`)

An earlier internal implementation extended the price leadership model to BLP
random-coefficients demand (`PriceLeadershipBLP`/`ple.blp()`). That extension
relied on `antitrust`'s internal (unexported) BLP integration and contraction
machinery. Rather than duplicate that engine or reach into `antitrust`'s
private namespace, `PriceLeadershipBLP` is deferred from this release pending
a small public `antitrust` interface for BLP mean-utility recovery. Standard
Logit PLE and Grim Trigger are unaffected.

## References

Mansley, Miller, Sheu & Weinberg (2023), "A price leadership model for
coordinated effects in merger analysis."
