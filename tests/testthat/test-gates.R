# Negative tests for the statistical gates in helper-gates.R.
#
# This is the file that makes the rest of them worth anything. An assertion
# helper is trivially satisfiable by an implementation that checks nothing, and a
# suite built on such a helper reports success while testing nothing at all. So
# each gate is exercised twice: once on input that satisfies its property, where
# it must stay silent, and once on input that violates it, where it must fail.
#
# The motivating case is real. The version these were ported from accepted either
# a count or a rate and guessed which:
#
#   observed <- if (successes > 1) successes / reps else successes # nolint
#
# One hit in four hundred replicates against a nominal 0.95 is a total failure of
# the procedure. One is not greater than one, so it was read as a rate of 1.0,
# which lies inside the band, and the assertion passed. The first test below is
# that exact case.

# testthat signals an expectation failure with stop(), so `expect_failure()` is
# the built-in way to assert that a gate rejected its input. Rolling our own with
# withCallingHandlers/muffleCondition does not work: the condition is not
# muffleable, and the attempt fails noisily rather than silently, which is at
# least honest.
expect_gate_fails <- testthat::expect_failure

# --------------------------------------------------------------------------
# The bug these gates exist not to repeat.
# --------------------------------------------------------------------------

test_that("a count of one is not a rate of one", {
  # The predecessor read this as coverage of 1.0 and passed it.
  expect_gate_fails(
    expect_rate_within_band(1, 400, 0.95, "catastrophic under-coverage")
  )
})

test_that("passing a count to the rate gate is an error, not a guess", {
  expect_error(
    expect_proportion_within_band(380, 400, 0.95, "count as rate"),
    "use expect_rate_within_band"
  )
})

test_that("a count above the replicate count is an error", {
  expect_error(expect_rate_within_band(401, 400, 0.95), "must be a single number")
})


# --------------------------------------------------------------------------
# Each gate: silent when the property holds, failing when it does not.
# --------------------------------------------------------------------------

test_that("a rate at nominal passes", {
  expect_rate_within_band(95, 100, 0.95, "calibrated")
  expect_proportion_within_band(0.95, 100, 0.95, "calibrated")
})

test_that("under-coverage fails", {
  expect_gate_fails(
    expect_proportion_within_band(0.80, 400, 0.95, "under-covering")
  )
})

test_that("over-coverage fails when the study is large enough to see it", {
  # At 100 replicates the 3-sigma upper bound clips at 1.0, so a vacuous
  # interval is undetectable; at 400 it is not. This is why the coverage study
  # in test-econometric-se-validation.R runs 400 replicates.
  expect_proportion_within_band(1.0, 100, 0.95, "vacuous, undetectable at 100")
  expect_gate_fails(
    expect_proportion_within_band(1.0, 400, 0.95, "vacuous interval")
  )
})

test_that("an unbiased sample passes and a shifted one fails", {
  set.seed(1)
  centered <- rnorm(400, mean = 2, sd = 0.5)
  expect_unbiased(centered, 2, "centered")

  shifted <- centered + 0.5
  expect_gate_fails(expect_unbiased(shifted, 2, "shifted by one sd"))
})

test_that("the bias gate tightens as the study grows", {
  # The property that makes a replicate-derived tolerance worth having: the same
  # call is lenient in a small study and strict in a large one, with no edit.
  set.seed(2)
  small <- rnorm(30, mean = 2.05, sd = 0.5)
  expect_unbiased(small, 2, "too small a study to resolve 0.05")

  large <- rnorm(20000, mean = 2.05, sd = 0.5)
  expect_gate_fails(expect_unbiased(large, 2, "large enough to resolve 0.05"))
})

test_that("a degenerate study is an error, not a pass", {
  # An estimator that never varies would otherwise divide by zero and could be
  # certified as unbiased on the strength of a single repeated number.
  expect_error(expect_unbiased(rep(1.0, 50), 1.0, "constant"), "did not vary")
  expect_error(expect_unbiased(c(1.0), 1.0, "single draw"), "at least two")
})


# --------------------------------------------------------------------------
# The band itself.
# --------------------------------------------------------------------------

test_that("the band narrows as 1/sqrt(reps)", {
  widths <- vapply(
    c(100, 400, 1600, 6400),
    function(r) diff(binomial_band(0.5, r)),
    numeric(1)
  )
  expect_true(all(diff(widths) < 0))
  # Four times the replicates halves the width. Asserting the rate, not merely
  # the direction. Measured at 0.5, where the band cannot clip.
  expect_equal(widths[1] / widths[2], 2, tolerance = 1e-9)
  expect_equal(widths[2] / widths[3], 2, tolerance = 1e-9)
})

test_that("the band stays inside [0, 1]", {
  band <- binomial_band(0.99, 25)
  expect_gte(band[1], 0)
  expect_lte(band[2], 1)
})

test_that("the band rejects impossible arguments", {
  expect_error(binomial_band(1.5, 100), "probability")
  expect_error(binomial_band(-0.1, 100), "probability")
  expect_error(binomial_band(0.95, 0), "positive")
  expect_error(binomial_band(0.95, 100, -1), "non-negative")
})


# --------------------------------------------------------------------------
# The replicate-count knob.
# --------------------------------------------------------------------------

test_that("mc_reps reads the environment and validates it", {
  # Base R rather than withr, which is not in this package's Suggests.
  original <- Sys.getenv("GUESS_MC_REPS", unset = NA)
  on.exit(
    if (is.na(original)) {
      Sys.unsetenv("GUESS_MC_REPS")
    } else {
      Sys.setenv(GUESS_MC_REPS = original)
    },
    add = TRUE
  )

  Sys.unsetenv("GUESS_MC_REPS")
  expect_equal(mc_reps(100), 100L)

  Sys.setenv(GUESS_MC_REPS = "400")
  expect_equal(mc_reps(100), 400L)

  Sys.setenv(GUESS_MC_REPS = "nonsense")
  expect_error(mc_reps(100), "positive integer")

  Sys.setenv(GUESS_MC_REPS = "0")
  expect_error(mc_reps(100), "positive integer")
})
