test_that("standard correction formula recovers true knowledge within sampling error", {
  set.seed(222)
  n <- 10000

  true_knowledge <- 0.40
  lucky <- 0.25

  knows <- rbinom(n, 1, true_knowledge)
  observed <- ifelse(knows == 1, 1, rbinom(n, 1, lucky))

  correct <- sum(observed == 1)
  incorrect <- sum(observed == 0)
  corrected <- (correct - incorrect / (1 / lucky - 1)) / n

  expect_equal(corrected, true_knowledge, tolerance = 0.02)
})

test_that("standard correction is unbiased across sample sizes", {
  set.seed(333)

  sample_sizes <- c(100, 500, 1000)
  true_knowledge <- 0.35
  lucky <- 0.20
  n_reps <- 50

  for (n in sample_sizes) {
    estimates <- numeric(n_reps)

    for (rep in seq_len(n_reps)) {
      knows <- rbinom(n, 1, true_knowledge)
      observed <- ifelse(knows == 1, 1, rbinom(n, 1, lucky))

      correct <- sum(observed == 1)
      incorrect <- sum(observed == 0)
      estimates[rep] <- (correct - incorrect / (1 / lucky - 1)) / n
    }

    bias <- mean(estimates) - true_knowledge

    expect_lt(abs(bias), 0.03)
  }
})

test_that("stnd_cor function matches manual formula application", {
  set.seed(444)
  n <- 500

  true_knowledge <- 0.45
  lucky <- 0.25

  knows <- rbinom(n, 1, true_knowledge)
  observed <- ifelse(knows == 1, 1, rbinom(n, 1, lucky))

  pre_test <- data.frame(item1 = observed)
  pst_test <- data.frame(item1 = observed)

  result <- stnd_cor(pre_test, pst_test, lucky = lucky)

  correct <- sum(observed == 1)
  incorrect <- sum(observed == 0)
  manual_corrected <- (correct - incorrect / (1 / lucky - 1)) / n

  expect_equal(as.numeric(result$pre[1]), manual_corrected, tolerance = 1e-10)
})

test_that("correction is close to true knowledge across lucky values", {
  set.seed(555)
  n <- 1000

  true_knowledge <- 0.30
  lucky_values <- c(0.20, 0.33, 0.50)

  knows <- rbinom(n, 1, true_knowledge)

  for (lucky in lucky_values) {
    observed <- ifelse(knows == 1, 1, rbinom(n, 1, lucky))

    correct <- sum(observed == 1)
    incorrect <- sum(observed == 0)
    estimate <- (correct - incorrect / (1 / lucky - 1)) / n

    expect_true(abs(estimate - true_knowledge) < 0.10)
  }
})

test_that("correction handles edge case of no incorrect answers", {
  pre_test <- data.frame(item1 = rep(1, 10))
  pst_test <- data.frame(item1 = rep(1, 10))

  result <- stnd_cor(pre_test, pst_test, lucky = 0.25)

  expect_equal(as.numeric(result$pre[1]), 1.0)
  expect_equal(as.numeric(result$pst[1]), 1.0)
})

test_that("correction handles balanced correct/incorrect", {
  pre_test <- data.frame(item1 = c(rep(1, 50), rep(0, 50)))
  pst_test <- data.frame(item1 = c(rep(1, 50), rep(0, 50)))

  result <- stnd_cor(pre_test, pst_test, lucky = 0.25)

  expected <- (50 - 50 / (1 / 0.25 - 1)) / 100

  expect_equal(as.numeric(result$pre[1]), expected, tolerance = 1e-10)
})

test_that("formula variance decreases with sample size", {
  set.seed(777)

  sample_sizes <- c(100, 1000)
  true_knowledge <- 0.40
  lucky <- 0.25
  n_reps <- 50

  variances <- numeric(length(sample_sizes))

  for (j in seq_along(sample_sizes)) {
    n <- sample_sizes[j]
    estimates <- numeric(n_reps)

    for (rep in seq_len(n_reps)) {
      knows <- rbinom(n, 1, true_knowledge)
      observed <- ifelse(knows == 1, 1, rbinom(n, 1, lucky))

      correct <- sum(observed == 1)
      incorrect <- sum(observed == 0)
      estimates[rep] <- (correct - incorrect / (1 / lucky - 1)) / n
    }

    variances[j] <- var(estimates)
  }

  expect_lt(variances[2], variances[1])
})
