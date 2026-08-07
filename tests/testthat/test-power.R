# Power of the goodness-of-fit test.
#
# The package had no power test of any kind: `grep -n "power" tests/testthat/`
# returned nothing. That matters more than it sounds. The Type I test in
# test-econometric-identification.R checks only that the GOF statistic does not
# reject *true* models too often, and the cheapest way to satisfy that is never
# to reject anything at all. Size and power have to be pinned together, or one
# of them can be bought with the other.
#
# The design perturbs the class mix handed to the GOF statistic away from the
# parameters that generated the data, by a known amount, so "more wrong" is a
# number rather than a judgement.
#
# Measured over 100 replicates at n=500, alpha=0.05:
#
#     shift 0.00  rejected   0/100  rate 0.000
#     shift 0.02  rejected   9/100  rate 0.090
#     shift 0.05  rejected 100/100  rate 1.000
#     shift 0.10  rejected 100/100  rate 1.000

# Fraction of replicates in which the GOF test rejects, when the fitted class
# mix is shifted `shift` away from the generating one.
#
# Returns both the rejection count and the number of replicates that converged,
# because a replicate lost to a solver failure is not evidence of anything and
# must not be silently counted as a non-rejection.
gof_rejections <- function(shift, n_sims, n = 500, alpha = 0.05, seed = 20260807) {
  set.seed(seed)
  rejected <- 0L
  converged <- 0L

  for (sim in seq_len(n_sims)) {
    data <- simulate_dk_prepost_data(n)
    trans <- multi_transmat(data$pre, data$post, force9 = TRUE)

    tryCatch(
      {
        result <- lca_cor(trans)
        params <- result$params[
          c("gg", "gk", "gd", "kk", "dg", "dk", "dd"), ,
          drop = FALSE
        ]
        # Move mass from "guessed both times" to "knew both times". At shift 0
        # this is the correctly specified model, which is what makes the first
        # row of the curve a size check rather than a power one.
        params["gg", ] <- pmax(params["gg", ] - shift, 1e-6)
        params["kk", ] <- params["kk", ] + shift

        fit <- fit_model(
          data$pre, data$post, result$params["gamma", ], params,
          force9 = TRUE
        )
        converged <- converged + 1L
        if (any(fit["p-value", ] < alpha, na.rm = TRUE)) {
          rejected <- rejected + 1L
        }
      },
      error = function(e) NULL
    )
  }

  list(rejected = rejected, converged = converged)
}


test_that("the GOF test rejects a misspecified class mix", {
  skip_on_cran()

  n_sims <- mc_reps(100L)
  result <- gof_rejections(0.10, n_sims)

  expect_gte(result$converged, 0.9 * n_sims)

  # A test with the right size and no power passes every other check in this
  # suite. Measured 100/100 at this shift; the gate is the 3-sigma lower bound
  # for a nominal 0.95, which is 0.885 at 100 replicates and tightens as the
  # replicate count rises.
  power <- result$rejected / result$converged
  expect_gte(power, binomial_band(0.95, result$converged)[1])
})


test_that("power rises with the size of the misspecification", {
  skip_on_cran()

  n_sims <- mc_reps(100L)
  shifts <- c(0.00, 0.02, 0.05)
  rates <- vapply(
    shifts,
    function(s) {
      r <- gof_rejections(s, n_sims)
      if (r$converged == 0) NA_real_ else r$rejected / r$converged
    },
    numeric(1)
  )

  expect_false(anyNA(rates))
  # Monotone: a test whose rejection rate did not rise with the distance from
  # the truth would not be measuring misspecification at all.
  expect_true(
    all(diff(rates) >= 0),
    label = paste0("rejection rates ", paste(sprintf("%.3f", rates), collapse = ", "))
  )

  # The correctly specified end is a size check, and must sit inside the band
  # around alpha. Measured 0.000.
  expect_lte(rates[1], binomial_band(0.05, n_sims)[2])

  # The far end must be high, or the curve is flat and monotonicity is vacuous.
  expect_gte(rates[length(rates)], binomial_band(0.95, n_sims)[1])
})
