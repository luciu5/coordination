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

Run this in a fresh R session. The shared iovrse installer installs
`coordination` with a compatible `antitrust` dependency.

```r
source(url("https://raw.githubusercontent.com/luciu5/competitiontoolbox/master/tools/install_iovrse.R"))
install_iovrse(packages = "coordination")
```

Use `install_iovrse()` for the complete cohort. Ordinary installation uses
version-compatible packages; release certification is described separately below.

## Available games

| Model | Public entry point | Demand and conduct |
| --- | --- | --- |
| Price leadership equilibrium | `ple()`, `ple.blp()` | Coordinated pricing with Logit or BLP demand |
| Grim Trigger sustainability | `calcProducerSurplusGrimTrigger()` | Repeated-game analysis on an existing antitrust Bertrand model |
| Firm-level Stackelberg | `stackelberg()` | Noncooperative leader/follower Logit or output CES, Bertrand or Cournot |
| Core-fringe | `core_fringe()` | A noncooperative strategic core and fringe, Logit or output CES |

The role games are separate from coordinated PLE and from antitrust's legacy
quantity Stackelberg model. Their examples use explicit firm and role IDs.

## Task guides

The source guides remain available while rendered documentation is prepared.
An ordinary installation does not require a document builder.

| Question | Guide |
| --- | --- |
| What do prices and margins identify about leadership? | [Prices, timing and identification](vignettes/price-leadership.Rmd) |
| Can participating firms sustain coordination? | [Grim Trigger](vignettes/grim-trigger.Rmd) |
| How do leader/core roles affect an ownership scenario? | [Noncooperative roles](vignettes/role-games.Rmd) |

The PLE guide deliberately starts from the existing zero-supermarkup boundary:
missing timing does not establish full incentive compatibility. Stackelberg and
core-fringe are separate noncooperative games. Always qualify
`coordination::stackelberg()` because antitrust exports a different namesake.
Logit role games use unconditional quantity shares; CES uses unconditional revenue
shares and inside expenditure. Input Logit Bertrand/Cournot can explicitly use
`price_domain = "real"` for signed counterfactual rates; CES remains output-only.

The [model reference](doc/coordination-model-reference.md) retains the longer
identification, portfolio and deterministic BLP notes. All teaching examples use
public APIs and in-memory inputs; no Bayesian fitting toolchain is required.

## Contributor release certification

The coordinated-release minimum is `antitrust >= 0.99.43`. Maintainers can select
**Release certification** in the workflow and supply a full 40-character
antitrust candidate SHA. That workflow verifies the selected source and records
requested and resolved revisions. Those certification inputs are separate from
the ordinary installation commands above.

## References

Mansley, Miller, Sheu & Weinberg (2023), "A price leadership model for
coordinated effects in merger analysis."
