# guess

Estimate learning from paired pre-test and post-test knowledge responses while
accounting for lucky guesses and explicit "don't know" responses.

[![R-CMD-check](https://github.com/finite-sample/guess/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/finite-sample/guess/actions/workflows/R-CMD-check.yml)
[![CRAN status](https://www.r-pkg.org/badges/version/guess)](https://CRAN.R-project.org/package=guess)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/guess)](https://cran.r-project.org/package=guess)

## Choose A Model

The package exposes separate models for separate estimands. Choose the model
before interpreting its output.

| Function | Estimand | Main assumption |
|---|---|---|
| `item_lca_fit()` | Proportion who learned each item | Each item has its own latent transition distribution |
| `person_item_lca_fit()` | Shared person-level trajectory and posterior probability of learning | A person has one `gg`, `gk`, or `kk` trajectory across all items |
| `lca_difficulty()` | Item-wise LCA with guessing expressed through a difficulty link | This is a reparameterized LCA, not an IRT model |
| `stnd_cor()` | Corrected pre, post, and gain scores | The guessing probability is supplied by the user |
| `group_adj()` | Guessing-adjusted group estimates | Guessing probabilities are supplied by group and item |

`estimate_logit_score()`, `cross_sectional_learning()`, and
`cross_sectional_learning_score()` are descriptive score baselines. They do
not fit Rasch or IRT models, and the bounded score is not a calibrated
probability of learning.

## Installation

```r
install.packages("guess")

# Development version
devtools::install_github("finite-sample/guess")
```

## Data Contract

Pass matching data frames with one row per person and one column per item.
Pre-test and post-test columns must have the same unique names. Valid response
codes are:

| Code | Meaning |
|---|---|
| `0` or `"0"` | Incorrect answer |
| `1` or `"1"` | Correct answer |
| `"d"` or `"DK"` | Observed "don't know" response |
| `NA` | Observed "don't know" by default |

Use `na_as = "missing"` when `NA` records a structural failure, such as an
item that was not shown or a response lost to a technical error. Incomplete
pre/post pairs are then omitted by default. Set `missing_action = "error"` to
reject them instead.

```r
fit <- item_lca_fit(pre_test, post_test, na_as = "missing")

fit <- item_lca_fit(
  pre_test,
  post_test,
  na_as = "missing",
  missing_action = "error"
)
```

Explicit `"d"` and `"DK"` values remain observed responses under either
setting. Structural missingness is not treated as a latent response class.

## Item-Level Learning

`item_lca_fit()` is the direct entry point for the model in Cor and Sood
(2016). It fits every item separately from its paired response transitions.

```r
library(guess)

item_sim <- simulate_lca(
  n = 1500,
  n_items = 4,
  gg = 0.40,
  gk = 0.30,
  kk = 0.30,
  gamma = c(0.15, 0.25, 0.35, 0.45),
  seed = 123
)

item_fit <- item_lca_fit(item_sim$pre, item_sim$post)
item_fit$learning
item_fit$params
```

For binary responses, the parameter rows are:

| Parameter | Meaning |
|---|---|
| `gg` | Guess at both waves |
| `gk` | Guess before, know after; the item-level learning estimand |
| `kk` | Know at both waves |
| `gamma` | Probability of a correct response while guessing |

`multi_transmat()` and `lca_cor()` expose the same workflow in two steps when
you already work with transition counts.

```r
transitions <- multi_transmat(item_sim$pre, item_sim$post)
count_fit <- lca_cor(transitions)
```

## Person-Level Learning

`person_item_lca_fit()` jointly uses all repeated items. It estimates shared
class proportions, item-specific guessing rates, and one posterior trajectory
for each person.

```r
person_sim <- simulate_lca(
  n = 1500,
  n_items = 5,
  gg = 0.35,
  gk = 0.35,
  kk = 0.30,
  gamma = 0.25,
  seed = 456,
  return_classes = TRUE
)

person_fit <- person_item_lca_fit(person_sim$pre, person_sim$post)
person_fit$class_priors
person_fit$gamma

posterior <- posterior_class_probs(person_fit)
p_learned <- posterior_learned(person_fit)
```

This model is useful only when one common trajectory across items is
substantively defensible. It does not allow the same person to know one item,
learn another, and remain ignorant on a third. Use `item_lca_fit()` when the
item-specific learning proportions are the target.

The person model currently supports binary responses but not the explicit DK
model.

## Don't-Know Responses

Observed DK responses select the nine-cell model. Its latent transition
parameters are `gg`, `gk`, `gd`, `kk`, `dg`, `dk`, and `dd`, plus `gamma`.
Learning is `gk + dk`: learning from a guessing state plus learning from an
observed don't-know state.

```r
dk_sim <- simulate_lca_dk(
  n = 1800,
  n_items = 3,
  gg = 0.25,
  gk = 0.15,
  gd = 0.10,
  kk = 0.15,
  dg = 0.10,
  dk = 0.10,
  dd = 0.15,
  gamma = 0.25,
  seed = 789
)

dk_fit <- item_lca_fit(dk_sim$pre, dk_sim$post)
dk_fit$learning
dk_fit$params
```

## Assumptions

The latent class correction relies on assumptions that should be reported with
the estimate:

1. People do not lose item knowledge over the interval. Know-to-guess and
   know-to-DK transitions are fixed to zero.
2. A person who knows an item answers it correctly. The current model has no
   slip parameter.
3. An item's guessing probability is stable across waves.
4. Structural missingness is ignorable when incomplete pairs are omitted.
5. The item-wise model treats items independently. The person model instead
   imposes one shared trajectory across items.

A correct-to-incorrect response is therefore attributed to guessing rather
than knowledge loss. That restriction identifies the learning parameters and
should be tested through sensitivity analysis when the interval is long or the
content can be forgotten.

## Diagnostics

The binary item model is saturated, so it has no residual degrees of freedom
for a goodness-of-fit test. The DK model has one over-identifying restriction,
and `fit_model()` reports its Pearson test.

```r
fit_stats <- fit_model(
  dk_sim$pre,
  dk_sim$post,
  g = dk_fit$params["gamma", ],
  est_param = dk_fit$params[-nrow(dk_fit$params), ],
  force9 = TRUE
)
```

Use held-out likelihood and perplexity to compare predictive performance.

```r
transitions <- multi_transmat(item_sim$pre, item_sim$post)
perplexity_items(item_fit, transitions)
perplexity_individuals(item_fit, item_sim$pre, item_sim$post)
cv_items(transitions, k = 4, seed = 321)
cv_individuals(item_sim$pre, item_sim$post, k = 5, seed = 321)
```

Validate recovery under sample sizes, item counts, class proportions, and
guessing rates that resemble the intended application.

```r
recovery <- validate_recovery(
  c(gg = 0.40, gk = 0.30, kk = 0.30, gamma = 0.25),
  n = 500,
  n_items = 4,
  n_sims = 100,
  seed = 654
)
recovery
```

## Longitudinal IRT

The package does not yet fit a longitudinal IRT model. The planned model will
estimate a population ability gain directly, constrain latent mastery to be
nondecreasing over the study interval, and retain item-specific guessing. It
will be exported separately only after simulation establishes identification,
parameter recovery, interval coverage, and agreement with standard
longitudinal IRT fits in compatible limiting cases.

## Documentation

```r
vignette("using_guess", package = "guess")
vignette("model_validation", package = "guess")
```

## Reference

Cor, K., and G. Sood. 2016. ["Guessing and Forgetting: A Latent Class Model for
Measuring Learning."](https://gsood.com/research/papers/guess.pdf) *Political
Analysis* 24(2): 226-242.

## License

MIT
