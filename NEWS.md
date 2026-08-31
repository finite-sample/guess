# version 0.8.0 2026-08-30

## Breaking Changes

* Replaced ambiguous model, transition, scoring, and cross-validation entry
  points with level-specific APIs: `fit_item_lca()`, `fit_person_lca()`,
  `count_transitions()`, `count_item_transitions()`, `score_item_lca()`,
  `score_individual_lca()`, and `cv_individual_lca()`. Superseded entry points
  have been removed.

* Raw-response functions now infer the binary or don't-know transition schema
  from the data. `na_as` only controls whether `NA` is an observed don't-know
  response or structural missingness.

* Standardized shared argument names to `pre_test`, `post_test`, and
  `guessing_probability`. `group_adj()` now uses
  `knowledge_given_dont_know` and returns `adjusted_responses` and
  `mean_learning`.

* `lca_se()` and `validate_recovery()` use an optional `seed`; `NULL` uses the
  current random-number-generator state, while an explicit seed is preserved.

## Bug Fixes

* Cross-sectional learning uses empirical-logit smoothing and returns `NA` for
  items with no observed responses.

* Bootstrap and cross-validation computations retain one inferred response
  schema across resamples and folds.

* DK model summaries and validation examples correctly identify learning as
  `gk + dk`.

* `validate_recovery()` now rejects degenerate generating models at the
  parameter boundary, where recovery diagnostics are not meaningful.

# version 0.7.0 2026-08-02

This release incorporates 0.6.0, which was tagged but never submitted to
CRAN, together with the further breaking changes below. The version on CRAN
is 0.3.0, so 0.4.0, 0.5.x and 0.6.0 are all superseded here.

## Breaking Changes

* **Model entry points now state their level and estimand.** `lca_fit()` is
  replaced by `item_lca_fit()` for independent item-wise fits.
  `person_item_lca_fit()` jointly estimates one shared latent trajectory per
  person, common class proportions, and item-specific guessing rates.
  `posterior_class_probs()` and `posterior_learned()` now extract results from
  that explicit person/item fit instead of silently fitting a different model
  from an item-wise result.

* **Functions that were not IRT models no longer claim to be.** `lca_irt()` is
  replaced by `lca_difficulty()` because it only reparameterizes the LCA
  guessing rate. `estimate_ability()` is replaced by `estimate_logit_score()`,
  its unsupported hand-built `"rasch"` branch is removed, and
  `cross_sectional_irt()` is replaced by `cross_sectional_learning_score()`.
  The latter is a descriptive bounded score, not a calibrated probability of
  learning.

* **Missing-response semantics are now explicit and consistent.** Every function
  accepting raw responses uses `na_as = c("dk", "missing")`. The default treats
  `NA` as an observed don't-know response and selects the nine-cell model; explicit
  `"d"`/`"DK"` always does the same. When `NA` is structural missingness,
  `missing_action = "omit"` excludes incomplete pairs and `"error"` rejects them.
  Structural missingness is not added to the latent-class model.

* **`validate_recovery()` no longer reports invalid confidence-interval coverage.**
  The function had used one Monte Carlo standard deviation as every replication's
  standard error, which is not replicate-specific interval coverage. It now reports
  bias, RMSE, and Monte Carlo SD, and averages estimates across every simulated item
  instead of silently keeping only the first.

* **The don't-know model now matches the model in the paper.** The likelihood
  implemented a different model from the one in Cor and Sood, equation (2). It kept the
  two latent classes the paper's identifying assumption sets to zero, dropped the two
  the paper keeps, and reused the survivors in the `d0` and `d1` cells:

  | cell | paper | what the code computed |
  |---|---|---|
  | `x10` | `gamma(1-gamma)*gg` | `(1-gamma)*gamma*gg + kg` |
  | `x11` | `gamma^2*gg + gamma*gk + kk` | `... + gamma*kg + kk` |
  | `x1d` | `gamma*gd` | `gamma*gd + kd` |
  | `xd0` | `(1-gamma)*dg` | `kg` |
  | `xd1` | `gamma*dg + dk` | `gamma*gk + kd` |

  In short, the know-to-guess class was conflated with don't-know-to-guess, and
  know-to-don't-know with don't-know-to-know.

* **DK parameters are renamed.** `kg` and `kd` are gone; `dg` (don't know -> guess) and
  `dk` (don't know -> know) take their place. The order is now `gg, gk, gd, kk, dg, dk,
  dd, gamma`. Code that indexes DK parameters by name or position must be updated.
  Learning is `gk + dk` -- those who learned the item from guessing, plus those who
  learned it from confessed ignorance. It was `gk + kd`, which the vignette rationalised
  as "true learning plus those who learned but lost confidence".

* **`simulate_lca_dk()` gains `dg` and `dk` arguments and loses `kg` and `kd`.** The
  simulator drew the same classes the likelihood named, so it generated knowledge loss
  and could never generate anyone who moved from a don't-know response to knowing --
  which is half of what the model defines as learning. No test could catch the
  likelihood bug while the simulator agreed with it.

* **`fit_model()` returns `NA` for data without don't-know responses.** That model has
  3 free parameters against 3 free cell probabilities. It is saturated, so there are no
  degrees of freedom and no test to report. It previously reported a p-value computed on
  `df = 3`, counting none of the parameters estimated from the same counts. For the DK
  model the degrees of freedom are now 1 rather than 8, which is what makes the test
  able to reject at all.

## Why this matters

The two consequences of the old cell equations were both invisible from outside:

* **They were not a distribution.** They summed to between 1.02 and 2.29 rather than 1,
  so the objective carried a spurious `-N log S(theta)` term worth up to ~182,000
  log-likelihood units, with no statistical content.

* **The parameters were not identified.** Fed exact model-implied counts with no
  sampling noise, the old estimator returned parameters off by up to 0.041 while the
  negative log-likelihood differed by 0.0009 in 1,741,431 -- observationally equivalent,
  so no dataset of any size could separate them. The same test now recovers every
  parameter to within 6e-05.

The paper's model is identified in closed form -- `gamma/(1-gamma)` is `x10/x00`, and
every lambda follows -- and over-identified by exactly one degree of freedom, the
restriction `x1d/x0d = x10/x00`.

## Bug Fixes

* **`stnd_cor()` now estimates paired learning from the same respondents in both
  corrected totals.** With wave-specific missingness, the previous implementation
  subtracted marginal pre- and post-test totals calculated from different respondent
  sets, then divided that unmatched difference by the number of complete pairs.
  Marginal pre- and post-test scores still use everyone observed at each wave; learning
  now uses complete pairs throughout and returns `NA` when no pair is observed.

* Zero-probability likelihood cells now contribute zero when their observed count is
  zero and infinite loss only when they are observed. Perplexity and cross-validation
  no longer discard impossible held-out observations or divide by unobserved pairs.

* Items with and without observed don't-know responses can now be combined safely:
  four-cell item transitions are promoted to the shared nine-cell schema with zero DK
  counts instead of being recycled into a malformed matrix.

* Tibble inputs now use vector-safe column extraction and produce the same transitions,
  fits, corrections, and missing-response behavior as data frames.

## Internal

* The nine DK and four no-DK cell probabilities had been written out in four separate
  places, each carrying the same error. They now come from single definitions,
  `dk_cell_probs()` and `nodk_cell_probs()`, which every likelihood, expected-count and
  goodness-of-fit routine calls.

## Testing

* End-to-end model-criticism tests verify that raw individual and aggregated item
  log-likelihoods and perplexities agree for binary, NA-coded DK, and structurally
  missing data. Additional tests cover cross-validation denominators, tibble pipelines,
  response-code validation, and simulation recovery.

* New `tests/testthat/test-dk-model-spec.R`: the cells sum to 1, the structural zeros
  hold, the closed-form inversion recovers every parameter, the over-identifying
  restriction holds, the estimator recovers the truth from exact counts, the simulator
  can produce learning from confessed ignorance, and the goodness-of-fit test spends
  1 degree of freedom.

* `test-econometric-likelihood.R` used to restate the cell equations inline and assert
  only that they were non-negative. They were, and they also summed to 2.29. It now
  calls the function the likelihood uses and checks that they sum to 1.

# version 0.5.1 2026-07-31

## Bug Fixes

* **`stnd_cor()` understated learning when responses were missing.** The numerators
  counted observed responses but the denominator was `nrow()`, so every item with
  missing data was shrunk toward zero in proportion to its missingness. On one item with
  true learning 0.384, a 10% missing rate returned 0.341 and a 40% rate returned 0.227 --
  a 41% understatement. Scores are now divided by the number of responses each item
  actually has, and `learn` by the respondents who answered at both waves. This is the
  bias the package exists to remove, so it mattered more than its size suggests.

* **`lca_se()` failed for most item counts.** A `resamp_agg` matrix was allocated
  `2 * n_items` wide while a transition row is 4 wide (or 9 with don't-know responses),
  and it indexed row `n_items` -- the last item -- rather than the aggregate row at
  `n_items + 1`. It errored outright on 3 and 5 items, and on every item count tried when
  the data contained DK responses, while silently recycling the row at 4 and 8 items.
  Nothing ever read the variable, so it has been removed. Bootstrapped standard errors
  now work for any item count, with or without DK.

* **Removed `R/fit_nodk.R` and `R/fit_dk.R`.** Both were shadowed at load time by the
  wrappers in `R/fit_unified.R`, which defines the same names later in alphabetical
  order, so the code never ran. The dead copies passed model-expected counts to
  `chisq.test()` as the observed vector and observed proportions as `p` -- had load order
  ever changed, they would have returned chi-square statistics between 0.83x and 2.08x
  the correct ones. `fit_nodk()` and `fit_dk()` remain exported and unchanged in
  behavior.

* **The optimiser no longer prints its iteration trace.** `solnp()` is called with
  `control = list(trace = 0)`, so `lca_fit()` and `lca_se()` are silent. A 100-resample
  bootstrap previously emitted hundreds of lines, which is how a genuine warning gets
  lost.

## Documentation

* Replaced the Unicode arrows and infinity signs in the `lca_difficulty()` and `simulate_lca()`
  documentation with ASCII. They produced LaTeX errors when building the PDF manual, so
  `R CMD check --as-cran` reported an ERROR and two WARNINGs on any machine with a
  working TeX installation.

## Testing

* New `tests/testthat/test-audit-regressions.R`: 22 tests covering the above, each of
  which fails against the previous release.

`R CMD check --as-cran` is clean: 0 errors, 0 warnings, and the one NOTE is the absence
of the optional `V8` package for math rendering.

# version 0.5.0 2026-04-05

## Breaking Changes
* **Parameter names simplified**: All parameter names have been changed to remove the confusing `l` prefix. The new naming pattern is `{pre_state}{post_state}`:
  - No-DK model: `lgg` → `gg`, `lgk` → `gk`, `lkk` → `kk` (gamma unchanged)
  - DK model: `lgg` → `gg`, `lgk` → `gk`, `lgd` → `gd`, `lkg` → `kg`, `lkk` → `kk`, `lkd` → `kd`, `ldd` → `dd`
  - This is a breaking change - update code that accesses parameters by name

## New Features
* **Simulation functions**: Added `simulate_lca()` and `simulate_lca_dk()` to generate data from known parameters for validation studies
* **Parameter recovery validation**: Added `validate_recovery()` for Monte Carlo validation of parameter estimates
* **Comprehensive validation tests**: New test files for parameter recovery, individual-level functions, and group adjustments

## Example Migration
```r
# Old code
result$params["lgk", ]
result$params[c("lgg", "lgk", "lkk"), ]

# New code
result$params["gk", ]
result$params[c("gg", "gk", "kk"), ]
```

# version 0.4.0 2026-04-05

## Breaking Changes
* **DK model parameter names fixed**: Parameter names in the Don't Know model now match the likelihood function:
  - Old: `lgg, lgk, lgc, lkk, lcg, lck, lcc, gamma`
  - New: `lgg, lgk, lgd, lkg, lkk, lkd, ldd, gamma`
  - This affects `lca_cor()` output when using DK data

## Improvements
* **Named parameter access**: All internal code now uses named row access (e.g., `params["gamma", ]`) instead of numeric indices, making the code more readable and robust to parameter reordering
* **GOF functions fixed**: `fit_model()`, `fit_dk()`, and `fit_nodk()` now use formulas consistent with the likelihood function
* **lca_adj improved**: Person-level adjustment function now correctly handles both DK and non-DK models with proper parameter names
* **Model criticism functions**: `cell_probs()` and `calculate_expected_values()` now use correct DK model formulas

## Bug Fixes
* **DK model cell probabilities**: Fixed formulas in `cell_probs()` to match the likelihood function exactly
* **Expected values calculation**: Fixed DK model expected values in `calculate_expected_values()`

# version 0.3.0 2026-03-30

## New Features
* **Comprehensive Econometric Correctness Test Suite**: Added extensive validation tests covering likelihood derivation, parameter identification, standard error computation, and parameter recovery across various sample sizes and true parameter configurations

## Bug Fixes
* **Chi-square GOF test**: Fixed argument handling for chi-square goodness-of-fit test
* **Expected values formula**: Corrected expected values computation in model fitting

## Testing
* **Simulation helpers**: Added `helper-simulation.R` with reusable functions for parameter recovery testing
* **Expanded test coverage**: 612 tests (up from ~275 in v0.2.2), 7 skipped (extended tests)
* **New test modules**:
  - `test-econometric-formula-derivation.R`: Validates likelihood formula components
  - `test-econometric-identification.R`: Tests parameter identification conditions
  - `test-econometric-likelihood.R`: Verifies likelihood computation correctness
  - `test-econometric-parameter-recovery.R`: Monte Carlo parameter recovery validation
  - `test-econometric-se-validation.R`: Standard error computation verification

# version 0.2.2 2025-12-15

## Validation System Modernization
* **Complete migration to checkmate**: Replaced all manual input validation with robust checkmate assertions
* **Enhanced validation utilities**: Added comprehensive validation helper functions in `utils-validation.R`
* **Standardized error messages**: All validation errors now use consistent checkmate format
* **Improved code quality**: Eliminated all manual `stop()` calls and `::` namespace patterns
* **Dependency optimization**: 
  - Added `checkmate` dependency for robust input validation
  - Removed `goji` dependency by implementing internal `zero1` function
* **Test suite updates**: Updated all test expectations to match new validation patterns
* **Documentation improvements**: Enhanced validation function documentation with proper `@importFrom` declarations

## Development Workflow Improvements
* **Local code coverage**: Replaced Codecov.io with simple local coverage reporting
  - Added `make coverage` command for quick coverage analysis
  - Created `Makefile` for common development tasks
  - Removed external Codecov dependency and badge
* **Fixed CRAN URL**: Updated to canonical CRAN package URL format

# version 0.2.1 2024-12-15

## Infrastructure & Modernization
* Added comprehensive GitHub Actions CI/CD workflows for R CMD check, test coverage, and pkgdown
* Updated minimum R version requirement to 4.0.0
* Modernized code patterns:
  - Replaced `T`/`F` with `TRUE`/`FALSE` throughout
  - Updated logical operators to use `||` for scalar comparisons
  - Removed deprecated `stringsAsFactors` arguments
  - Added explicit parameters to `mapply()` calls
* Enhanced package metadata in DESCRIPTION
* Improved .gitignore and .Rbuildignore patterns
* Re-enabled and configured lintr for code quality checks
* Updated pkgdown configuration URLs

# version 0.2.0 2017-05-XX

* Consistent support for input data format (with potential for d for 'don't know').
* Person level adjustments for LCA and standard correction
* Explain logic for Rsolnp priors and allow people to pass different priors
* Standard output and nomenclature for stnd_cor and lca_cor, including option for s.e.
* Extensive linting, expect_lint_free passes
