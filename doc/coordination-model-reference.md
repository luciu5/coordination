# Coordination and role games: model reference

This reference supplements the short [PLE identification](../vignettes/price-leadership.Rmd),
[Grim Trigger](../vignettes/grim-trigger.Rmd) and [role-game](../vignettes/role-games.Rmd)
guides. It documents existing behavior; it does not introduce new economic APIs.

## Price leadership and identification

The framework follows Mansley, Miller, Sheu and Weinberg (2023), “A price leadership
model for coordinated effects in merger analysis,” as cited in the public help.
A coalition's leadership profit, deviation profit and Bertrand punishment profit
enter firm-level slack

\[
g_f=(\pi_f^{PL}-\pi_f^D)+\frac{\delta_f}{1-\delta_f}(\pi_f^{PL}-\pi_f^B).
\]

Full IC requires g_f>=0 at valid supplied or identified timing. With unidentified
timing, calcSlack warns and reports only the immediate payoff difference. The
zero-supermarkup example in the starter is an identification boundary. It is not
an independently validated demonstration of positive leadership or full IC.
Supplying timing makes a scenario assumption; it does not turn that assumption
into a calibrated estimate. Canonical firm identifiers used by timing outputs
can differ from original character ownership labels; inspect the public ownership
mapping before interpreting named timing parameters.

The constructor stores prices solved in the constrained regime; calcPrices defaults
to coordination. Always name the regime when comparing prices. The supermarkup
method's reference differs between pre/post settings: existing tests explicitly
cover a zero post unconstrained comparison. Do not interpret that zero as a generic
finding that collusion is absent, or every observed/reference difference as a
leadership supermarkup. Observed and fitted prices can differ after calibration.

## Grim Trigger portfolio comparison

With a common factor delta in a firm portfolio, sum single-period coordinated,
deviation and punishment profit as C,D,P. Continued coordination is preferred
when C/(1-delta) exceeds D+delta*P/(1-delta). Full portfolios matter even when only
some products appear in the coalition index. Product-level factors are processed
inside the actual portfolio comparison; a scalar threshold must not be applied
without its common-factor assumption. Current implementation uses strict IC in
the pre state and weak IC in the post state at equality; toy sensitivity examples
avoid exact indifference. isCollusion=TRUE changes the calibration assumption and
recalculates demand/cost, not just an output label.

## Noncooperative roles and rate domains

Stackelberg is sequential: leaders anticipate strategic followers. Core-fringe is
simultaneous with strategic core and atomistic fringe conditions. Neither is a
coordinating PLE coalition. Both use explicit firm IDs and require post roles when
ownership changes. Logit uses unconditional quantity shares and inside quantity;
CES uses unconditional revenue shares and inside expenditure, with positive
outside-good price. CES input markets are unsupported.

The shared validation code and independent signed-rate tests support price_domain
real for input Logit Bertrand and Cournot in both games. Output real-domain and
CES real-domain requests are rejected. This concerns counterfactual rates;
constructor observations and remaining model conditions still apply. At zero rates,
normalized margins can be undefined while dollar markdowns remain meaningful.
Signed revenue totals can also make revenue-share normalizations undefined.

FOC residuals establish numerical stationarity to a scale/tolerance; they do not
prove global uniqueness, existence outside the solved cell or empirical adequacy.
Follower response matrices are derivatives of the follower equilibrium with respect
to leader actions, not causal estimates from data. Native retention changes alter
portfolio profit weights; effective-cost changes must be supplied consistently.
trade performs the explicit physical/effective-cost conversion for its supported
promotion route.

## Deterministic BLP extension

The existing README recipe uses public ple.blp and Gauss–Hermite integration, with
pre-calibrated BLP parameters. The following code is an advanced recipe; the short
PLE guide intentionally does not require it.

```r
shares <- c(.35, .25, .25, .15)
prices <- c(.93, .88, 1.10, 1.02)
alpha <- -5.767013
fit <- coordination::ple.blp(
  prices = prices, shares = shares,
  ownerPre = c("Bank1", "Bank2", "Bank3", "Fringe"),
  ownerPost = c("Bank1", "Bank2", "Bank3", "Fringe"),
  coalitionPre = 1:3, coalitionPost = 1:3, insideSize = 1000,
  integration = "gauss-hermite",
  slopes = list(alphaMean = alpha, alpha = alpha, sigma = .5,
    meanval = c(0, log(shares[-1]/shares[1]) - alpha*(prices[-1]-prices[1])),
    sigmaNest = 1)
)
```

Gauss–Hermite and supplied-node rules are deterministic. Monte Carlo integration
requires explicit RNG/seed provenance; it is not needed by these teaching guides.
No inference, HMC or bridge sampling is implied by a deterministic price solve.
