## guess: Adjust Estimates of Learning for Guessing

[![R-CMD-check](https://github.com/finite-sample/guess/workflows/R-CMD-check/badge.svg)](https://github.com/finite-sample/guess/actions)
[![Codecov test coverage](https://codecov.io/gh/finite-sample/guess/branch/master/graph/badge.svg)](https://app.codecov.io/gh/finite-sample/guess?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/guess)](https://cran.r-project.org/package=guess)
![](http://cranlogs.r-pkg.org/badges/grand-total/guess)
[![Github Stars](https://img.shields.io/github/stars/finite-sample/guess.svg?style=social&label=Github)](https://github.com/finite-sample/guess)

Over informative processes, naive estimator of learning---difference between post and pre process scores---underestimates actual learning. A heuristic account for why the naive estimator is negatively biased is as follows: people know as much or more after exposed to an informative process than before it. And the less people know, the larger the number of items they don't know. And greater the opportunity to guess. 

Guessing, even when random, only increases the proportion correct. Thus, bias due to guessing for naive measures of knowledge is always positive. On average, thus, there is more positive bias in the pre-process scores than post-process scores. And naturally, subtracting pre-process scores from post-process provides an attenuated estimate of actual learning. For a more complete treatment of the issue, read [this paper](https://gsood.com/research/papers/guess.pdf) by Ken Cor and Gaurav Sood.

We provide a few different ways to adjust estimates of learning for guessing. For now, we limit our attention to cases where the same battery of knowledge questions has been asked in both the pre- and the post-process wave. And to cases where closed-ended questions have been asked. (Guessing is not a serious issue on open-ended items. See more evidence for that in [DK Means DK](https://johnbullock.org/papers/DKs/DK.pdf) by Robert Luskin and John Bullock.)  More generally, the package implements the methods to adjust learning for guessing discussed in [this paper](https://gsood.com/research/papers/guess.pdf).

### Installation

To get the current release version from CRAN: 
```r
install.packages("guess")
```

To get the current development version from GitHub:

```r
# install.packages("devtools")
library(devtools)
devtools::install_github("finite-sample/guess", build_vignettes = TRUE)
```

### Usage

To learn about how to use the package, see the [vignette](vignettes/using_guess.Rmd):
```r
# Overview of the package
vignette("using_guess", package = "guess")
```

### Key Functions

The package provides several methods for adjusting learning estimates:

- **`lca_cor()`**: Latent Class Analysis correction using transition matrices
- **`stnd_cor()`**: Standard guessing correction based on number of incorrect responses  
- **`group_adj()`**: Group-level adjustment accounting for propensity to guess
- **`fit_model()`**: Unified model fitting function with goodness-of-fit testing

### Handling Groups

**Important**: The `group_adj()` function handles different groups implicitly through their **gamma values** (propensity to guess), rather than requiring explicit group identifiers. Groups with different guessing behaviors should use different gamma parameters:

```r
# Example: Different groups with different guessing rates
high_ability_gamma <- 0.15   # Lower guessing rate
low_ability_gamma <- 0.35    # Higher guessing rate

# Apply adjustment with group-specific gamma
group_adj(pre_test, post_test, gamma = c(high_ability_gamma, low_ability_gamma, ...))
```

This design allows for flexible modeling where gamma can vary by item, respondent characteristics, or any other grouping structure.

### License
Scripts are released under [MIT License](https://opensource.org/licenses/MIT).

### Contributor Code of Conduct

The project welcomes contributions from everyone! In fact, it depends on it. To maintain this welcoming atmosphere, and to collaborate in a fun and productive way, we expect contributors to the project to abide by the [Contributor Code of Conduct](https://www.contributor-covenant.org/version/1/0/0/).
