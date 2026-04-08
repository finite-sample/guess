# guess: Adjust Estimates of Learning for Guessing

[![R-CMD-check](https://github.com/finite-sample/guess/workflows/R-CMD-check/badge.svg)](https://github.com/finite-sample/guess/actions)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/guess)](https://CRAN.R-project.org/package=guess)
![](http://cranlogs.r-pkg.org/badges/grand-total/guess)

## The Problem

When measuring learning from pre/post tests, the naive estimate (post score minus pre score) **underestimates** actual learning.

**Why?** People who don't know an answer may guess correctly. Since there's more to learn before an intervention than after, there's more guessing on pre-tests. This inflates pre-test scores more than post-test scores, making learning gains appear smaller than they actually are.

## The Solution

This package provides methods to correct for guessing bias in learning estimates:

| Method | Function | Best For |
|--------|----------|----------|
| **Latent Class Model** | `lca_cor()` | Most accurate; uses transition patterns |
| **Standard Correction** | `stnd_cor()` | Quick estimate when you know the guess rate |
| **Group Adjustment** | `group_adj()` | When guessing varies by group/item |

## Installation

```r
# From CRAN
install.packages("guess")

# Development version
# install.packages("devtools")
devtools::install_github("finite-sample/guess")
```

## Quick Start

```r
library(guess)

# Your pre and post test data (0 = wrong, 1 = correct)
pre_test <- data.frame(
  item1 = c(1, 0, 0, 1, 0, 1, 0, 0),
  item2 = c(0, 0, 1, 1, 0, 0, 1, 0)
)
post_test <- data.frame(
  item1 = c(1, 1, 0, 1, 1, 1, 0, 1),
  item2 = c(1, 0, 1, 1, 1, 0, 1, 1)
)

# Method 1: Latent Class Correction (recommended)
result <- lca_fit(pre_test, post_test)
result$learning  # Corrected learning estimates per item

# Method 2: Standard Correction
# For 4-option multiple choice, guess rate = 0.25
stnd_cor(pre_test, post_test, lucky = c(0.25, 0.25))$learn
```

## Main Functions

### `lca_fit()` / `lca_cor()` - Latent Class Model

The most sophisticated correction. Uses the pattern of transitions (wrong→right, right→right, etc.) to estimate:
- **Learning**: Proportion who truly learned
- **Guessing rate** (gamma): Probability of guessing correctly

```r
# Direct approach
result <- lca_fit(pre_test, post_test)

# Or step by step
trans_matrix <- multi_transmat(pre_test, post_test)
result <- lca_cor(trans_matrix)

# Access results
result$learning                    # Learning estimates
result$params["gamma", ]           # Guessing rates by item
result$params["gk", ]              # "Learned" parameter (guess→know)
```

### `stnd_cor()` - Standard Correction

Quick correction when you know the guessing probability (e.g., 1/4 for 4-option MC):

```r
stnd_cor(pre_test, post_test, lucky = c(0.25, 0.25))
# Returns: $pre (adjusted pre), $pst (adjusted post), $learn (learning)
```

### `fit_model()` - Goodness of Fit

Test whether the LCA model fits your data:

```r
result <- lca_fit(pre_test, post_test)
gof <- fit_model(pre_test, post_test,
                 result$params["gamma", ],
                 result$params[c("gg", "gk", "kk"), ])
# High p-values indicate good fit
```

## Handling "Don't Know" Responses

If your test includes a "Don't Know" option, code it as `"d"`:

```r
pre_dk <- data.frame(item1 = c("1", "0", "d", "1", "d"))
post_dk <- data.frame(item1 = c("1", "1", "1", "d", "0"))

# Force 9-column transition matrix for DK model
trans <- multi_transmat(pre_dk, post_dk, force9 = TRUE)
result <- lca_cor(trans)

# DK model has 8 parameters: gg, gk, gd, kg, kk, kd, dd, gamma
```

## Understanding the Parameters

Parameter names follow the pattern `{pre_state}{post_state}` where:
- `g` = guessing (don't know)
- `k` = know
- `d` = don't know response

### No-DK Model (4 parameters)
| Parameter | Meaning |
|-----------|---------|
| `gg` | Proportion: guess→guess (stable ignorance) |
| `gk` | Proportion: guess→know (**LEARNED**) |
| `kk` | Proportion: know→know (stable knowledge) |
| `gamma` | Probability of guessing correctly |

### DK Model (8 parameters)
| Parameter | Meaning |
|-----------|---------|
| `gg` | guess→guess |
| `gk` | guess→know (learned) |
| `gd` | guess→dk |
| `kg` | know→guess (forgot) |
| `kk` | know→know |
| `kd` | know→dk |
| `dd` | dk→dk |
| `gamma` | Guessing probability |

## Simulation and Validation

Before trusting results on real data, validate that the model recovers parameters under conditions similar to yours:

```r
# 1. Define your assumptions:
#    - Expected learning rate (~30%)
#    - Guessing probability (0.25 for 4-option MC)
#    - Your sample size

# 2. Run validation
results <- validate_recovery(
  c(gg = 0.40, gk = 0.30, kk = 0.30, gamma = 0.25),
  n = 500,        # your expected sample size
  n_sims = 100    # number of simulations
)

# 3. Check results
print(results)
#   parameter  true_value  mean_estimate  bias    rmse    coverage_95
#   gk         0.30        0.301          0.001   0.042   0.94

# Bias < 0.05 and coverage ~95%? Proceed with confidence.
```

This is useful when:
- Sample size is small (can the model handle n=100?)
- Parameters are extreme (what if 70% already know?)
- Planning a study (what n gives acceptable precision?)

For single simulations:
```r
sim <- simulate_lca(n = 500, gg = 0.35, gk = 0.30, kk = 0.35, gamma = 0.25)
fit <- lca_fit(sim$pre, sim$post)
fit$params["gk", ]  # Should be close to 0.30
```

## Individual-Level Learning Recovery

Beyond aggregate learning rates, you can estimate which specific individuals learned using `posterior_learned()`. This computes P(learned | data) for each person using the LCA model's joint transition structure.

```r
sim <- simulate_lca(n = 500, n_items = 5, gk = 0.30, seed = 123, return_classes = TRUE)
fit <- lca_fit(sim$pre, sim$post)

# LCA posterior: P(learned | data) per individual
p_learned_lca <- posterior_learned(fit, sim$pre, sim$post)

# Cross-sectional IRT: ability difference (ignores transition structure)
p_learned_cs <- cross_sectional_irt(sim$pre, sim$post)

# Compare recovery of true learning status
cor(p_learned_lca, sim$learned)  # ~0.99
cor(p_learned_cs, sim$learned)   # ~0.75
```

| Method | Correlation with Truth | Why? |
|--------|----------------------|------|
| LCA (`posterior_learned`) | ~0.99 | Uses joint pre→post transitions |
| Cross-sectional IRT | ~0.75 | Ignores transition structure |

The LCA model wins because it uses the full transition matrix (wrong→right, right→right, etc.) to separate true learners from lucky guessers. Cross-sectional methods only see ability at each timepoint separately.

For systematic Monte Carlo validation of these results, see:
```r
vignette("model_validation", package = "guess")
```

## More Examples

See the vignette for detailed examples:
```r
vignette("using_guess", package = "guess")
```

## References

Cor, K., & Sood, G. (2018). [Adjusting Estimates of Learning for Guessing](https://gsood.com/research/papers/guess.pdf).

## License

MIT
