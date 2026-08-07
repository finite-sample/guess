#' Statistical gates whose tolerance comes from the replicate count.
#'
#' An assertion like `expect_true(coverage >= 0.80)` for a nominal 0.95 interval
#' is arbitrary. At 100 replicates the Monte Carlo standard error of a coverage
#' estimate is about 0.022, so 0.80 sits seven standard errors below nominal: a
#' procedure that truly covers 82% of the time passes. And at ten thousand
#' replicates the same line is far too loose to mean anything at all.
#'
#' Every gate here derives its band from the number of replicates instead, so
#' tightening a test means running more replicates rather than editing a
#' constant, and a failure reports how far outside the band the result landed.
#'
#' Ported from `simcheck` (https://github.com/finite-sample/simcheck), which was
#' extracted from the same fleet's Python packages. guess is R, so it cannot
#' depend on simcheck directly; this is the same arithmetic in R.
#'
#' Counts and rates are separate functions on purpose. simcheck's predecessor
#' accepted either and guessed which it had been given:
#'
#'     observed <- if (successes > 1) successes / reps else successes
#'
#' For a nominal 0.95 study, one hit in four hundred replicates -- a total
#' failure of the procedure -- is not greater than one, so it was read as a
#' *rate* of 1.0, which sits inside the band, and the assertion passed. The worst
#' possible result was reported as the best possible one. No heuristic separates
#' a count of 1 from a rate of 1.0; the caller has to say which.

# How many standard errors a result may sit from nominal before it counts as
# miscalibrated. Three is loose enough that a correct procedure essentially never
# trips it -- about one false failure in 370 -- and tight enough to catch a
# meaningful miscalibration at a few hundred replicates.
GATE_SIGMAS <- 3


#' The interval a well-calibrated rate should land in.
#'
#' @param nominal The rate the procedure claims, e.g. 0.95 for coverage.
#' @param reps Number of replicates.
#' @param sigmas How many binomial standard errors of slack to allow.
#' @return Numeric vector `c(low, high)`, clipped to `[0, 1]`.
binomial_band <- function(nominal, reps, sigmas = GATE_SIGMAS) {
  if (!is.numeric(nominal) || length(nominal) != 1 || is.na(nominal) ||
    nominal < 0 || nominal > 1) {
    stop("nominal must be a single probability in [0, 1], got ", nominal)
  }
  if (!is.numeric(reps) || length(reps) != 1 || is.na(reps) || reps <= 0) {
    stop("reps must be a single positive number, got ", reps)
  }
  if (!is.numeric(sigmas) || length(sigmas) != 1 || is.na(sigmas) || sigmas < 0) {
    stop("sigmas must be a single non-negative number, got ", sigmas)
  }
  spread <- sigmas * sqrt(nominal * (1 - nominal) / reps)
  c(max(0, nominal - spread), min(1, nominal + spread))
}


#' Fail if an observed **rate** is inconsistent with the claimed one.
#'
#' @param observed The observed rate, in `[0, 1]`. Pass a count to
#'   [expect_rate_within_band()] instead.
#' @param reps Number of replicates the rate was computed over.
#' @param nominal The claimed rate.
#' @param label Included in the failure message.
#' @param sigmas Slack, in binomial standard errors.
#' @return Invisibly, `observed`.
expect_proportion_within_band <- function(observed, reps, nominal, label = "",
                                          sigmas = GATE_SIGMAS) {
  if (!is.numeric(observed) || length(observed) != 1 || is.na(observed) ||
    observed < 0 || observed > 1) {
    stop(
      "observed must be a rate in [0, 1], got ", observed,
      ". If this is a count of successes, use expect_rate_within_band()."
    )
  }
  band <- binomial_band(nominal, reps, sigmas)
  testthat::expect_true(
    observed >= band[1] && observed <= band[2],
    info = sprintf(
      "%s: observed rate %.4f outside the %g-sigma band [%.4f, %.4f] for a nominal %.4f over %d replicates",
      label, observed, sigmas, band[1], band[2], nominal, as.integer(reps)
    )
  )
  invisible(observed)
}


#' Fail if a **count** of successes is inconsistent with the claimed rate.
#'
#' @param successes Number of successes, in `[0, reps]`.
#' @param reps Number of replicates.
#' @param nominal The claimed rate.
#' @param label Included in the failure message.
#' @param sigmas Slack, in binomial standard errors.
#' @return Invisibly, the implied rate.
expect_rate_within_band <- function(successes, reps, nominal, label = "",
                                    sigmas = GATE_SIGMAS) {
  if (!is.numeric(successes) || length(successes) != 1 || is.na(successes) ||
    successes < 0 || successes > reps) {
    stop("successes must be a single number in [0, ", reps, "], got ", successes)
  }
  expect_proportion_within_band(successes / reps, reps, nominal, label, sigmas)
}


#' Fail if a set of estimates is distinguishable from the truth.
#'
#' The comparison is against the *Monte Carlo* standard error of the mean,
#' `sd / sqrt(R)`, so a bias too small for the study to resolve does not fail,
#' and the study is made more demanding by running more replicates rather than
#' by editing a threshold.
#'
#' @param estimates Numeric vector, one estimate per replicate.
#' @param truth The true value being estimated.
#' @param label Included in the failure message.
#' @param sigmas How many Monte Carlo standard errors of slack to allow.
#' @return Invisibly, the bias.
expect_unbiased <- function(estimates, truth, label = "", sigmas = GATE_SIGMAS) {
  finite <- estimates[is.finite(estimates)]
  if (length(finite) < 2) {
    stop(
      label, ": need at least two finite estimates to test unbiasedness, got ",
      length(finite)
    )
  }
  bias <- mean(finite) - truth
  spread <- stats::sd(finite)
  if (spread == 0) {
    stop(
      label, ": the estimator did not vary across ", length(finite),
      " replicates, so there is no sampling variation to test the bias against"
    )
  }
  mc_se <- spread / sqrt(length(finite))
  t_stat <- bias / mc_se

  testthat::expect_true(
    abs(t_stat) < sigmas,
    info = sprintf(
      "%s: bias %+.6f is %+.2f Monte Carlo standard errors from zero over %d replicates (sd %.6f, mc se %.6f)",
      label, bias, t_stat, length(finite), spread, mc_se
    )
  )
  invisible(bias)
}


#' Fail if a bias is large relative to the Monte Carlo error of the study.
#'
#' The same test as [expect_unbiased()], for callers that have summary
#' statistics rather than the estimates themselves -- `validate_recovery()`
#' returns a bias and a standard *deviation*, so the standard error of the mean
#' has to be formed here as `sd / sqrt(reps)`.
#'
#' @param bias Observed mean estimate minus the truth.
#' @param sd Standard deviation of the estimates across replicates.
#' @param reps Number of replicates behind those two numbers.
#' @param label Included in the failure message.
#' @param sigmas How many Monte Carlo standard errors of slack to allow.
#' @return Invisibly, the t statistic.
expect_bias_within_mc_error <- function(bias, sd, reps, label = "",
                                        sigmas = GATE_SIGMAS) {
  if (!is.numeric(sd) || length(sd) != 1 || is.na(sd) || sd <= 0) {
    stop(
      label, ": need a positive standard deviation to judge the bias against, got ",
      sd
    )
  }
  if (!is.numeric(reps) || length(reps) != 1 || is.na(reps) || reps < 2) {
    stop(label, ": need at least two replicates, got ", reps)
  }
  mc_se <- sd / sqrt(reps)
  t_stat <- bias / mc_se
  testthat::expect_true(
    abs(t_stat) < sigmas,
    info = sprintf(
      "%s: bias %+.6f is %+.2f Monte Carlo standard errors from zero over %d replicates (sd %.6f, mc se %.6f)",
      label, bias, t_stat, as.integer(reps), sd, mc_se
    )
  )
  invisible(t_stat)
}


#' Replicate count for the current tier.
#'
#' Reading this from the environment is what lets a scheduled job run a deeper
#' study than a laptop without any test being edited. Because the gates above
#' derive their tolerance from the count, raising it makes every assertion
#' stricter -- and a test cannot be quietly weakened by lowering it, since the
#' band widens visibly in the failure message.
#'
#' @param default Replicates to use when `GUESS_MC_REPS` is unset.
#' @return Integer replicate count.
mc_reps <- function(default = 100L) {
  raw <- Sys.getenv("GUESS_MC_REPS", "")
  if (!nzchar(raw)) {
    return(as.integer(default))
  }
  parsed <- suppressWarnings(as.integer(raw))
  if (is.na(parsed) || parsed <= 0) {
    stop("GUESS_MC_REPS must be a positive integer, got ", raw)
  }
  parsed
}
