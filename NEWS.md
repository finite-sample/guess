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
  behaviour.

* **The optimiser no longer prints its iteration trace.** `solnp()` is called with
  `control = list(trace = 0)`, so `lca_fit()` and `lca_se()` are silent. A 100-resample
  bootstrap previously emitted hundreds of lines, which is how a genuine warning gets
  lost.

## Documentation

* Replaced the Unicode arrows and infinity signs in the `lca_irt()` and `simulate_lca()`
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