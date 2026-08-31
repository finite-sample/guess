# Tests for posterior class probability and learning recovery functions

# Local helper for Monte Carlo learning comparison (test-only)
compare_learning_recovery_test <- function(n, n_items, n_sims, seed = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)

  results <- data.frame(
    cor_lca = numeric(n_sims),
    cor_cs = numeric(n_sims),
    lca_advantage = numeric(n_sims)
  )

  for (s in seq_len(n_sims)) {
    sim_data <- simulate_lca(n = n, n_items = n_items, ..., return_classes = TRUE)
    true_learned <- as.numeric(sim_data$learned)

    tryCatch(
      {
        fit <- fit_person_lca(sim_data$pre, sim_data$post)
        p_learned_lca <- posterior_learned(fit)
        p_learned_cs <- cross_sectional_learning_score(sim_data$pre, sim_data$post)

        results$cor_lca[s] <- cor(p_learned_lca, true_learned, use = "complete.obs")
        results$cor_cs[s] <- cor(p_learned_cs, true_learned, use = "complete.obs")
        results$lca_advantage[s] <- results$cor_lca[s] - results$cor_cs[s]
      },
      error = function(e) {
        results$cor_lca[s] <- NA_real_
        results$cor_cs[s] <- NA_real_
        results$lca_advantage[s] <- NA_real_
      }
    )
  }
  results
}

test_that("simulate_lca returns true classes when requested", {
  sim <- simulate_lca(n = 100, gk = 0.30, seed = 123, return_classes = TRUE)

  expect_true("true_class" %in% names(sim))
  expect_true("learned" %in% names(sim))
  expect_true(is.factor(sim$true_class))
  expect_equal(levels(sim$true_class), c("gg", "gk", "kk"))
  expect_true(is.logical(sim$learned))
  expect_equal(length(sim$true_class), 100)
  expect_equal(length(sim$learned), 100)
})

test_that("simulate_lca learned matches gk class", {
  sim <- simulate_lca(n = 100, gk = 0.30, seed = 456, return_classes = TRUE)

  expect_equal(sim$learned, sim$true_class == "gk")
})

test_that("simulate_lca class proportions are approximately correct", {
  sim <- simulate_lca(
    n = 5000, gg = 0.40, gk = 0.35, kk = 0.25,
    seed = 789, return_classes = TRUE
  )

  props <- table(sim$true_class) / 5000
  expect_true(abs(props["gg"] - 0.40) < 0.05)
  expect_true(abs(props["gk"] - 0.35) < 0.05)
  expect_true(abs(props["kk"] - 0.25) < 0.05)
})

test_that("simulate_lca without return_classes has no class info", {
  sim <- simulate_lca(n = 100, seed = 123)

  expect_false("true_class" %in% names(sim))
  expect_false("learned" %in% names(sim))
  expect_true(all(c("pre", "post") %in% names(sim)))
})

test_that("posterior_class_probs returns valid probabilities", {
  sim <- simulate_lca(n = 50, n_items = 3, gk = 0.30, seed = 123, return_classes = TRUE)
  fit <- fit_person_lca(sim$pre, sim$post)

  posteriors <- posterior_class_probs(fit)

  expect_equal(nrow(posteriors), 50)
  expect_equal(ncol(posteriors), 3)
  expect_equal(names(posteriors), c("P_gg", "P_gk", "P_kk"))

  expect_true(all(posteriors >= 0))
  expect_true(all(posteriors <= 1))

  row_sums <- rowSums(posteriors)
  expect_true(all(abs(row_sums - 1) < 1e-10))
})

test_that("posterior extractors reject malformed fitted objects", {
  sim <- simulate_lca(n = 30, n_items = 2, seed = 321)
  fit <- fit_person_lca(sim$pre, sim$post)

  expect_error(posterior_class_probs(list()), "guess_person_fit")
  fit$posterior$P_gk[1] <- 1.1
  expect_error(posterior_class_probs(fit), "probabilities between 0 and 1")
})

test_that("posterior_learned returns vector of correct length", {
  sim <- simulate_lca(n = 50, n_items = 3, gk = 0.30, seed = 123)
  fit <- fit_person_lca(sim$pre, sim$post)

  p_learned <- posterior_learned(fit)

  expect_length(p_learned, 50)
  expect_true(all(p_learned >= 0))
  expect_true(all(p_learned <= 1))
})

test_that("posterior_learned correlates with true learning status", {
  sim <- simulate_lca(
    n = 500, n_items = 5, gk = 0.30, gamma = 0.25,
    seed = 123, return_classes = TRUE
  )
  fit <- fit_person_lca(sim$pre, sim$post)

  p_learned <- posterior_learned(fit)
  cor_with_truth <- cor(p_learned, as.numeric(sim$learned))

  expect_true(cor_with_truth > 0.3)
})

test_that("estimate_logit_score returns valid scores", {
  sim <- simulate_lca(n = 100, n_items = 5, seed = 123)

  score_pre <- estimate_logit_score(sim$pre)
  score_post <- estimate_logit_score(sim$post)

  expect_length(score_pre, 100)
  expect_length(score_post, 100)
  expect_true(all(is.finite(score_pre)))
  expect_true(all(is.finite(score_post)))
})

test_that("estimate_logit_score uses a denominator-aware empirical logit", {
  responses <- data.frame(
    item1 = c(0, 1, 0, NA),
    item2 = c(0, 1, 1, NA)
  )

  score <- estimate_logit_score(responses, na_as = "missing")
  expected <- qlogis(c(0.5 / 3, 2.5 / 3, 1.5 / 3, NA_real_))

  expect_equal(unname(score), expected)
  expect_lt(abs(score[1]), abs(qlogis(0.0001)))
  expect_true(is.na(score[4]))
})

test_that("cross_sectional_learning returns difference in logit scores", {
  sim <- simulate_lca(n = 100, n_items = 3, seed = 123)

  learning_cs <- cross_sectional_learning(sim$pre, sim$post)

  score_pre <- estimate_logit_score(sim$pre)
  score_post <- estimate_logit_score(sim$post)

  expect_equal(learning_cs, score_post - score_pre)
})

test_that("cross-sectional learning aligns post-test items by name", {
  pre_test <- data.frame(i1 = c(1, NA), i2 = c(0, 1))
  post_test <- data.frame(i1 = c(1, 0), i2 = c(1, 1))

  original <- cross_sectional_learning(
    pre_test, post_test, na_as = "missing"
  )
  reordered <- cross_sectional_learning(
    pre_test, post_test[c("i2", "i1")], na_as = "missing"
  )

  expect_equal(original, reordered)
  names(post_test)[1] <- "wrong"
  expect_error(
    cross_sectional_learning(pre_test, post_test),
    "same item names"
  )
})

test_that("cross_sectional_learning_score returns values in [0,1]", {
  sim <- simulate_lca(n = 100, n_items = 3, seed = 123)

  learning_score <- cross_sectional_learning_score(sim$pre, sim$post)

  expect_length(learning_score, 100)
  expect_true(all(learning_score >= 0))
  expect_true(all(learning_score <= 1))
})

test_that("cross-sectional scale must be finite and nonzero", {
  sim <- simulate_lca(n = 10, n_items = 2, seed = 741)

  expect_error(
    cross_sectional_learning_score(sim$pre, sim$post, scale = 0),
    "scale must be nonzero"
  )
  expect_error(
    cross_sectional_learning_score(sim$pre, sim$post, scale = Inf),
    "scale"
  )
  expect_error(
    cross_sectional_learning_score(sim$pre, sim$post, scale = NA_real_),
    "scale"
  )
})

test_that("LCA outperforms cross-sectional for learning recovery", {
  skip_on_cran()

  sim <- simulate_lca(
    n = 1000, n_items = 5, gk = 0.30, gamma = 0.25,
    seed = 123, return_classes = TRUE
  )
  fit <- fit_person_lca(sim$pre, sim$post)

  p_learned_lca <- posterior_learned(fit)
  p_learned_cs <- cross_sectional_learning_score(sim$pre, sim$post)
  true_learned <- as.numeric(sim$learned)

  cor_lca <- cor(p_learned_lca, true_learned)
  cor_cs <- cor(p_learned_cs, true_learned)

  expect_true(cor_lca > cor_cs)
})

test_that("class_conditional_item returns correct likelihoods", {
  gamma <- 0.25

  ccl_00 <- guess:::class_conditional_item(0, 0, gamma)
  expect_equal(ccl_00["gg"], (1 - gamma)^2, ignore_attr = TRUE)
  expect_equal(ccl_00["gk"], 0, ignore_attr = TRUE)
  expect_equal(ccl_00["kk"], 0, ignore_attr = TRUE)

  ccl_01 <- guess:::class_conditional_item(0, 1, gamma)
  expect_equal(ccl_01["gg"], gamma * (1 - gamma), ignore_attr = TRUE)
  expect_equal(ccl_01["gk"], 1 - gamma, ignore_attr = TRUE)
  expect_equal(ccl_01["kk"], 0, ignore_attr = TRUE)

  ccl_10 <- guess:::class_conditional_item(1, 0, gamma)
  expect_equal(ccl_10["gg"], gamma * (1 - gamma), ignore_attr = TRUE)
  expect_equal(ccl_10["gk"], 0, ignore_attr = TRUE)
  expect_equal(ccl_10["kk"], 0, ignore_attr = TRUE)

  ccl_11 <- guess:::class_conditional_item(1, 1, gamma)
  expect_equal(ccl_11["gg"], gamma^2, ignore_attr = TRUE)
  expect_equal(ccl_11["gk"], gamma, ignore_attr = TRUE)
  expect_equal(ccl_11["kk"], 1, ignore_attr = TRUE)
})

test_that("kk class individuals get high P_kk posterior", {
  sim <- simulate_lca(
    n = 500, gg = 0.33, gk = 0.34, kk = 0.33, n_items = 5,
    seed = 123, return_classes = TRUE
  )
  fit <- fit_person_lca(sim$pre, sim$post)

  posteriors <- posterior_class_probs(fit)

  kk_indices <- which(sim$true_class == "kk")

  mean_p_kk_for_kk <- mean(posteriors$P_kk[kk_indices])
  mean_p_kk_for_others <- mean(posteriors$P_kk[-kk_indices])

  expect_true(mean_p_kk_for_kk > mean_p_kk_for_others)
})

test_that("gk class individuals get moderate P_gk posterior", {
  sim <- simulate_lca(
    n = 500, gg = 0.33, gk = 0.34, kk = 0.33, n_items = 5,
    seed = 456, return_classes = TRUE
  )
  fit <- fit_person_lca(sim$pre, sim$post)

  p_learned <- posterior_learned(fit)

  gk_indices <- which(sim$true_class == "gk")
  other_indices <- which(sim$true_class != "gk")

  mean_p_gk_for_gk <- mean(p_learned[gk_indices])
  mean_p_gk_for_others <- mean(p_learned[other_indices])

  expect_true(mean_p_gk_for_gk > mean_p_gk_for_others)
})

test_that("Monte Carlo: LCA consistently outperforms cross-sectional", {
  skip_on_cran()

  results <- compare_learning_recovery_test(
    n = 500, n_items = 5, n_sims = 20,
    gk = 0.30, gamma = 0.25, seed = 42
  )

  valid <- !is.na(results$lca_advantage)
  expect_true(sum(valid) >= 15)

  mean_advantage <- mean(results$lca_advantage[valid])
  expect_true(mean_advantage > 0)

  prop_lca_wins <- mean(results$lca_advantage[valid] > 0)
  expect_true(prop_lca_wins > 0.7)
})

test_that("Monte Carlo: person-level LCA wins across guessing rates", {
  skip_on_cran()

  results_low_gamma <- compare_learning_recovery_test(
    n = 500, n_items = 5, n_sims = 10,
    gk = 0.30, gamma = 0.10, seed = 100
  )

  results_high_gamma <- compare_learning_recovery_test(
    n = 500, n_items = 5, n_sims = 10,
    gk = 0.30, gamma = 0.40, seed = 200
  )

  valid_low <- !is.na(results_low_gamma$lca_advantage)
  valid_high <- !is.na(results_high_gamma$lca_advantage)

  mean_adv_low <- mean(results_low_gamma$lca_advantage[valid_low])
  mean_adv_high <- mean(results_high_gamma$lca_advantage[valid_high])
  mean_lca_low <- mean(results_low_gamma$cor_lca[valid_low])
  mean_lca_high <- mean(results_high_gamma$cor_lca[valid_high])

  expect_true(mean_adv_low > 0)
  expect_true(mean_adv_high > 0)
  expect_true(mean_lca_low > mean_lca_high)
})

test_that("person-level posterior estimates shared class proportions", {
  sim <- simulate_lca(
    n = 3000, n_items = 5,
    gg = 0.40, gk = 0.30, kk = 0.30,
    gamma = 0.25, seed = 731
  )
  fit <- fit_person_lca(sim$pre, sim$post)
  posterior <- posterior_class_probs(fit)

  expect_equal(posterior, fit$posterior)
  expect_equal(
    fit$class_priors,
    c(gg = 0.40, gk = 0.30, kk = 0.30),
    tolerance = 0.04
  )
  expect_lte(fit$iterations, 1000L)
})
