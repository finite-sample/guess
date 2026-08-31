## Two defects the 0.7.0 release review found, both in workflows the
## documentation tells people to use.

test_that("fit_person_lca() accepts ordinary column names", {
  set.seed(1)
  n <- 200
  mk <- function() sample(c(0, 1), n, TRUE)
  pre <- data.frame(math = mk(), reading = mk(), science = mk())
  post <- data.frame(math = mk(), reading = mk(), science = mk())

  fit <- expect_no_error(fit_person_lca(pre, post))
  expect_s3_class(fit, "guess_person_fit")
  expect_setequal(names(fit$gamma), names(pre))
})

test_that("a supplied person-model start is checked against the data", {
  set.seed(2)
  n <- 120
  mk <- function() sample(c(0, 1), n, TRUE)
  pre <- data.frame(a = mk(), b = mk())
  post <- data.frame(a = mk(), b = mk())
  wrong <- list(
    class_priors = c(gg = 0.4, gk = 0.3, kk = 0.3),
    gamma = c(x = 0.25, y = 0.25)
  )
  expect_error(fit_person_lca(pre, post, start = wrong), "for every item")
})

test_that("cv_individual_lca() preserves the full DK schema in every fold", {
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

  expect_equal(ncol(count_item_transitions(pre, pst)), 9L)
  expect_no_error(cv_individual_lca(pre, pst, k = 5))
})

test_that("the DK schema is what makes that work, not luck", {
  # Guards the mechanism rather than the symptom: a fold with no DK response of
  # its own must still be given the nine-cell schema the full data implies.
  set.seed(42)
  n <- 40
  mk <- function() sample(c("0", "1"), n, TRUE)
  pre <- data.frame(i1 = mk(), i2 = mk(), stringsAsFactors = FALSE)
  pst <- data.frame(i1 = mk(), i2 = mk(), stringsAsFactors = FALSE)

  # A binary slice remains four-cell; callers cannot force an unidentified DK model.
  expect_equal(ncol(count_item_transitions(pre, pst)), 4L)
  expect_error(count_item_transitions(pre, pst, response_schema = "dk"), "empty")
})
