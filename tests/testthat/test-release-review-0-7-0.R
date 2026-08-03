## Two defects the 0.7.0 release review found, both in workflows the
## documentation tells people to use.

test_that("person_item_lca_fit() accepts ordinary column names", {
  # multi_transmat() labels items positionally as item1..itemN and drops the
  # source column names, so the initializer person_item_lca_fit() builds for
  # itself never matched the data's own names: calling it on a data frame with
  # columns like math/reading/science failed with "item_fit item names must
  # match the response data." -- the function rejecting its own default. It
  # only worked if the columns were already named itemN.
  set.seed(1)
  n <- 200
  mk <- function() sample(c(0, 1), n, TRUE)
  pre <- data.frame(math = mk(), reading = mk(), science = mk())
  post <- data.frame(math = mk(), reading = mk(), science = mk())

  fit <- expect_no_error(person_item_lca_fit(pre, post))
  expect_s3_class(fit, "guess_person_fit")
  # The per-item guessing rates come back labelled with the caller's names,
  # not item1..itemN.
  expect_setequal(names(fit$gamma), names(pre))
})

test_that("a supplied item_fit is still checked against the data", {
  # The positional alignment applies only to the initializer built internally.
  # A fit the caller passes in is theirs to get right, and a mismatch there is
  # a real error rather than an artefact of internal naming.
  set.seed(2)
  n <- 120
  mk <- function() sample(c(0, 1), n, TRUE)
  pre <- data.frame(a = mk(), b = mk())
  post <- data.frame(a = mk(), b = mk())
  other <- data.frame(x = mk(), y = mk(), z = mk())

  wrong <- item_lca_fit(other, other, na_as = "missing", missing_action = "omit")
  expect_error(person_item_lca_fit(pre, post, item_fit = wrong))
})

test_that("cv_individuals() survives DK responses too sparse to reach every fold", {
  # When the full data contain DK but a training fold does not -- ordinary when
  # DK is rare, or when the only DK respondents are the ones held out -- the
  # training matrix came back with four cells and a no-DK model was fitted.
  # Scoring that fold then met a held-out "d" and raised "Don't know responses
  # require a DK model." outside the fold's tryCatch, aborting the whole run
  # rather than degrading that one fold.
  set.seed(42)
  n <- 60
  mk <- function() sample(c("0", "1"), n, TRUE)
  pre <- data.frame(i1 = mk(), i2 = mk(), stringsAsFactors = FALSE)
  pst <- data.frame(i1 = mk(), i2 = mk(), stringsAsFactors = FALSE)
  # every DK response belongs to one respondent
  pre$i1[1] <- "d"
  pre$i2[1] <- "d"
  pst$i1[1] <- "d"

  expect_equal(ncol(multi_transmat(pre, pst)), 9L)
  expect_no_error(cv_individuals(pre, pst, k = 5))
})

test_that("the DK schema is what makes that work, not luck", {
  # Guards the mechanism rather than the symptom: a fold with no DK response of
  # its own must still be given the nine-cell schema the full data implies.
  set.seed(42)
  n <- 40
  mk <- function() sample(c("0", "1"), n, TRUE)
  pre <- data.frame(i1 = mk(), i2 = mk(), stringsAsFactors = FALSE)
  pst <- data.frame(i1 = mk(), i2 = mk(), stringsAsFactors = FALSE)

  # A slice with no DK at all is four-cell by default, nine-cell when forced.
  expect_equal(ncol(multi_transmat(pre, pst)), 4L)
  expect_equal(ncol(multi_transmat(pre, pst, force9 = TRUE)), 9L)
})
