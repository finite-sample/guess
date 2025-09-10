## guess: Adjust Estimates of Learning for Guessing

[![Build status](https://ci.appveyor.com/api/projects/status/ug1n0n8d9589arr5?svg=true)](https://ci.appveyor.com/project/soodoku/guess)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/guess)](https://cran.r-project.org/package=guess)
![](http://cranlogs.r-pkg.org/badges/grand-total/guess)
[![Research software impact](http://depsy.org/api/package/cran/guess/badge.svg)](http://depsy.org/package/r/guess)
[![Github Stars](https://img.shields.io/github/stars/soodoku/guess.svg?style=social&label=Github)](https://github.com/soodoku/guess)

Over informative processes, naive estimator of learning---difference between post and pre process scores---underestimates actual learning. A heuristic account for why the naive estimator is negatively biased is as follows: people know as much or more after exposed to an informative process than before it. And the less people know, the larger the number of items they don't know. And greater the opportunity to guess. 

Guessing, even when random, only increases the proportion correct. Thus, bias due to guessing for naive measures of knowledge is always positive. On average, thus, there is more positive bias in the pre-process scores than post-process scores. And naturally, subtracting pre-process scores from post-process provides an attenuated estimate of actual learning. For a more complete treatment of the issue, read [this paper](http://gsood.com/research/papers/guess.pdf) by Ken Cor and Gaurav Sood.

We provide a few different ways to adjust estimates of learning for guessing. For now, we limit our attention to cases where the same battery of knowledge questions has been asked in both the pre- and the post-process wave. And to cases where closed-ended questions have been asked. (Guessing is not a serious issue on open-ended items. See more evidence for that in [DK Means DK](http://johnbullock.org/papers/DKs/DK.pdf) by Robert Luskin and John Bullock.)  More generally, the package implements the methods to adjust learning for guessing discussed in [this paper](http://gsood.com/research/papers/guess.pdf).

### Installation

To get the current release version from CRAN: 
```r
install.packages("guess")
```

To get the current development version from GitHub:

```r
# install.packages("devtools")
library(devtools)
devtools::install_github("soodoku/guess", build_vignettes = TRUE)
```

### Usage

To learn about how to use the package, see the [vignette](vignettes/using_guess.Rmd):
```r
# Overview of the package
vignette("using_guess", package = "guess")
```

### License
Scripts are released under [MIT License](https://opensource.org/licenses/MIT).

### Contributor Code of Conduct

The project welcomes contributions from everyone! In fact, it depends on it. To maintain this welcoming atmosphere, and to collaborate in a fun and productive way, we expect contributors to the project to abide by the [Contributor Code of Conduct](http://contributor-covenant.org/version/1/0/0/).
