## Submission (v0.7.0)

This release corrects two estimation bugs, both of which biased results in the
direction the package exists to remove, and renames the model entry points so
each states its level and estimand. It supersedes 0.4.0, 0.5.x and 0.6.0, which
were developed but never submitted; the version on CRAN is 0.3.0, so this
submission spans several NEWS entries.

The don't-know latent class model implemented a different set of cell
probabilities from the model in the paper the package cites (Cor and Sood,
equation 2). Two latent classes that the paper's identifying assumption sets to
zero were retained, the two it retains were dropped, and the survivors were
reused in the wrong cells. The consequences were that the cell probabilities
summed to between 1.02 and 2.29 rather than 1, and that the parameters were not
identified. The corrected model sums to 1 and recovers every parameter from
exact model-implied counts to within 6e-05; the previous code was off by up to
0.041 at any sample size.

Separately, `stnd_cor()` divided by `nrow()` while its numerators used
`na.rm = TRUE`, so items with missing data were shrunk toward zero in
proportion to their missingness -- a 40% missing rate understated learning by
41%.

Full detail is in NEWS.md and in
https://github.com/finite-sample/guess/issues/1.

## Breaking changes

The model entry points are renamed so each states what it fits. `lca_fit()`
becomes `item_lca_fit()` for independent item-wise fits, and
`person_item_lca_fit()` jointly estimates one shared latent trajectory per
person; `posterior_class_probs()` and `posterior_learned()` now read from that
explicit fit rather than silently fitting a different model from an item-wise
result. `lca_irt()` becomes `lca_difficulty()`, `estimate_ability()` becomes
`estimate_logit_score()` (its unsupported hand-built "rasch" branch is removed),
and `cross_sectional_irt()` becomes `cross_sectional_learning_score()` -- none
of the three was an IRT model, and the last is a descriptive bounded score
rather than a calibrated probability.

Missing-response handling is now explicit: every function taking raw responses
accepts `na_as = c("dk", "missing")`, and `missing_action` selects whether
structural missingness is omitted or rejected.

Users of the don't-know model must update parameter names. `kg` (know to guess)
and `kd` (know to don't know) are removed, since the model's identifying
assumption sets both to zero; `dg` (don't know to guess) and `dk` (don't know to
know) take their place. The parameter order is now
`gg, gk, gd, kk, dg, dk, dd, gamma`, and the learning estimate is `gk + dk`.

`simulate_lca_dk()` takes `dg` and `dk` in place of `kg` and `kd`.

`fit_model()` now returns `NA` for data without don't-know responses. That model
has three free parameters against three free cell probabilities, so it is
saturated and no goodness-of-fit test is possible; it previously reported a
p-value computed on degrees of freedom that counted none of the parameters
estimated from the same data.

There are no reverse dependencies on CRAN.

## Test environments

* local macOS (Darwin 25.5.0), R 4.6.0
* GitHub Actions (ubuntu-latest): R-devel, R-release, R-oldrel-1
* GitHub Actions (windows-latest): R-release
* GitHub Actions (macOS-latest): R-release

## R CMD check results

0 errors | 0 warnings | 1 note

The note is local only: "Skipping checking math rendering: package 'V8'
unavailable". It reflects V8 not being installed on the machine that ran the
check, not anything in the package.

All five GitHub Actions configurations pass.

## Testing

3981 tests pass, including `tests/testthat/test-dk-model-spec.R` that pins
the corrected model: the cell probabilities sum to 1, the structural zeros hold,
the closed-form inversion recovers every parameter, the over-identifying
restriction holds, and the estimator recovers the truth from exact counts.
