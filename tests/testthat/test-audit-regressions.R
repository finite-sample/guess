# Regression tests for the defects found in the 2026-07-31 audit.
#
# Each of these fails against the code as it stood before its fix. They are
# grouped by defect so a future regression points straight at what broke.
#
# The recurring theme is worth stating: two of the three defects pushed learning
# estimates *downward*, which is the exact bias this package exists to remove,
# and neither was visible from the outside.

# ---------------------------------------------------------------------------
# Defect 1: stnd_cor divided by nrow() instead of the number of responses
# ---------------------------------------------------------------------------

test_that("stnd_cor matches a hand computation with no missing data", {
  # 5 people, one item, 4 options so the penalty is wrong/(4-1).
  pre <- data.frame(item1 = c(1, 0, 0, 1, 0))
  pst <- data.frame(item1 = c(1, 1, 1, 1, 0))

  res <- stnd_cor(pre, pst, guessing_probability = 0.25)

  # pre: 2 correct, 3 wrong -> 2 - 3/3 = 1, over 5 respondents
  expect_equal(res$pre[[1]], (2 - 3 / 3) / 5)
  # pst: 4 correct, 1 wrong -> 4 - 1/3, over 5 respondents
  expect_equal(res$pst[[1]], (4 - 1 / 3) / 5)
  expect_equal(res$learn[[1]], res$pst[[1]] - res$pre[[1]])
})

test_that("stnd_cor divides by responses, not rows, when data are missing", {
  # item2 carries one NA, so it has 4 responses across 5 rows.
  pre <- data.frame(item1 = c(1, 0, 0, 1, 0), item2 = c(1, NA, 0, 1, 0))
  pst <- data.frame(item1 = c(1, 1, 1, 1, 0), item2 = c(1, NA, 1, 1, 0))

  res <- stnd_cor(
    pre, pst,
    guessing_probability = rep(0.25, 2), na_as = "missing"
  )

  # item2 pre: 2 correct, 2 wrong among the 4 who answered.
  expect_equal(res$pre[[2]], (2 - 2 / 3) / 4)
  # Dividing by nrow() would have given (2 - 2/3)/5 = 0.2667.
  expect_false(isTRUE(all.equal(res$pre[[2]], (2 - 2 / 3) / 5)))
})

test_that("stnd_cor learning estimates do not shrink with the missing rate", {
  # The defect scaled `learn` by (responses / rows), so a 40% missing rate cut
  # the estimate by 41%. Learning is a property of the people who answered, so
  # dropping respondents at random must not move it systematically.
  set.seed(1)
  n <- 4000
  pre_full <- rbinom(n, 1, 0.30)
  pst_full <- pmin(pre_full + rbinom(n, 1, 0.40), 1)

  complete <- stnd_cor(
    data.frame(i = pre_full), data.frame(i = pst_full),
    guessing_probability = 0.25
  )$learn[[1]]

  for (rate in c(0.10, 0.25, 0.40)) {
    a <- pre_full
    b <- pst_full
    drop <- sample(n, floor(rate * n))
    a[drop] <- NA
    b[drop] <- NA
    got <- stnd_cor(
      data.frame(i = a), data.frame(i = b),
      guessing_probability = 0.25,
      na_as = "missing"
    )$learn[[1]]

    # Generous tolerance: this asserts the absence of systematic shrinkage, not
    # the absence of sampling noise from which rows were dropped.
    expect_equal(got, complete, tolerance = 0.06)
  }
})

test_that("stnd_cor handles pre and post missing on different rows", {
  # Marginal wave scores use everyone observed at that wave. Learning uses only
  # complete pairs in both its corrected totals and denominator.
  pre <- data.frame(item1 = c(1, 0, NA, 1, 0))
  pst <- data.frame(item1 = c(1, 1, 1, NA, 0))

  res <- stnd_cor(pre, pst, guessing_probability = 0.25, na_as = "missing")

  expect_equal(res$pre[[1]], (2 - 2 / 3) / 4) # 4 answered the pre-test
  expect_equal(res$pst[[1]], (3 - 1 / 3) / 4) # 4 answered the post-test
  expect_equal(res$learn[[1]], ((2 - 1 / 3) - (1 - 2 / 3)) / 3)
})

test_that("stnd_cor does not mix unmatched corrected totals", {
  pre <- data.frame(item1 = c(1, 0))
  pst <- data.frame(item1 = c(NA, 1))

  res <- stnd_cor(pre, pst, guessing_probability = 0.25, na_as = "missing")

  expect_equal(res$pre[[1]], (1 - 1 / 3) / 2)
  expect_equal(res$pst[[1]], 1)
  expect_equal(res$learn[[1]], 1 - (-1 / 3))
})

test_that("stnd_cor paired learning matches a simulation oracle", {
  set.seed(991)
  n <- 5000
  pre <- rbinom(n, 1, 0.35)
  pst <- pmax(pre, rbinom(n, 1, 0.30))
  pre[sample.int(n, 700)] <- NA
  pst[sample.int(n, 1100)] <- NA
  complete <- !is.na(pre) & !is.na(pst)

  res <- stnd_cor(
    data.frame(item1 = pre), data.frame(item1 = pst),
    guessing_probability = 0.25,
    na_as = "missing"
  )
  corrected_mean <- function(x) {
    (sum(x == 1) - sum(x == 0) / 3) / length(x)
  }
  oracle <- corrected_mean(pst[complete]) - corrected_mean(pre[complete])

  expect_equal(res$learn[[1]], oracle)
})

test_that("stnd_cor returns NA without an observed denominator", {
  res <- stnd_cor(
    data.frame(item1 = c(1, NA)),
    data.frame(item1 = c(NA, 1)),
    guessing_probability = 0.25,
    na_as = "missing"
  )

  expect_true(is.na(res$learn[[1]]))
})

# ---------------------------------------------------------------------------
# Defect 2: lca_se errored for most item counts
# ---------------------------------------------------------------------------

make_pre_post <- function(n_items, n = 120, dk = FALSE, seed = 42) {
  set.seed(seed)
  pre <- as.data.frame(matrix(rbinom(n * n_items, 1, 0.35), ncol = n_items))
  pst <- as.data.frame(
    matrix(pmin(as.matrix(pre) + rbinom(n * n_items, 1, 0.40), 1), ncol = n_items)
  )
  names(pre) <- paste0("item", seq_len(n_items))
  names(pst) <- paste0("item", seq_len(n_items))
  if (dk) {
    for (j in seq_len(n_items)) {
      pre[[j]] <- as.character(pre[[j]])
      pst[[j]] <- as.character(pst[[j]])
      pre[sample(n, 15), j] <- "d"
    }
  }
  list(pre = pre, pst = pst)
}

test_that("lca_se works for item counts other than two", {
  # It errored at 3 and 5 items with "number of items to replace is not a
  # multiple of replacement length", from a dead variable nothing ever read.
  for (k in c(3, 5)) {
    d <- make_pre_post(k)
    expect_silent({
      res <- suppressWarnings(
        utils::capture.output(r <- lca_se(d$pre, d$pst, n_resamples = 3))
      )
    })
    expect_length(as.numeric(r$learning_standard_error), k + 1)
    expect_true(all(is.finite(as.numeric(r$learning_standard_error))))
  }
})

test_that("lca_se works when the data carry don't-know responses", {
  # This failed for every item count tried, and the DK model is one of the
  # package's headline features.
  for (k in c(2, 3)) {
    d <- make_pre_post(k, dk = TRUE)
    utils::capture.output(r <- suppressWarnings(lca_se(d$pre, d$pst, n_resamples = 3)))
    expect_length(as.numeric(r$learning_standard_error), k + 1)
    expect_true(all(is.finite(as.numeric(r$learning_standard_error))))
  }
})

test_that("lca_se standard errors shrink as the sample grows", {
  # A sanity check on the bootstrap itself, not just that it runs.
  small <- make_pre_post(2, n = 60, seed = 7)
  large <- make_pre_post(2, n = 600, seed = 7)

  utils::capture.output(s <- suppressWarnings(lca_se(small$pre, small$pst, n_resamples = 25)))
  utils::capture.output(l <- suppressWarnings(lca_se(large$pre, large$pst, n_resamples = 25)))

  expect_lt(
    mean(as.numeric(l$learning_standard_error)),
    mean(as.numeric(s$learning_standard_error))
  )
})

test_that("lca_se has no hidden seed and preserves an explicit seed state", {
  d <- make_pre_post(2, n = 200, seed = 321)
  expect_null(formals(lca_se)$seed)

  set.seed(654)
  before <- get(".Random.seed", envir = .GlobalEnv)
  first <- lca_se(d$pre, d$pst, n_resamples = 3, seed = 987)
  after <- get(".Random.seed", envir = .GlobalEnv)
  second <- lca_se(d$pre, d$pst, n_resamples = 3, seed = 987)

  expect_identical(before, after)
  expect_equal(first, second)
})
