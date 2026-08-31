test_that("fit_item_lca has a stable raw-data contract", {
  expect_named(
    formals(fit_item_lca),
    c(
      "pre_test", "post_test", "...", "subgroup", "include_aggregate",
      "na_as", "missing_action", "start", "control"
    )
  )
  expect_false(exists("item_lca_fit", envir = asNamespace("guess"), inherits = FALSE))

  pre_test <- data.frame(item = c(0, 1))
  post_test <- data.frame(item = c(1, 1))
  expect_error(fit_item_lca(pre_test, post_test, NULL), "must be empty")
})

test_that("fit_item_lca agrees with the explicit binary workflow", {
  sim <- simulate_lca(
    n = 1200, n_items = 3,
    gg = 0.40, gk = 0.30, kk = 0.30,
    gamma = c(0.15, 0.25, 0.35), seed = 801
  )
  subgroup <- seq_len(nrow(sim$pre)) %% 2L == 0L

  fit <- fit_item_lca(
    sim$pre,
    sim$post,
    subgroup = subgroup,
    include_aggregate = TRUE
  )
  transition_counts <- count_item_transitions(
    sim$pre,
    sim$post,
    subgroup = subgroup,
    include_aggregate = TRUE
  )
  explicit <- fit_item_lca_counts(transition_counts)

  expect_equal(fit$params, explicit$params)
  expect_equal(fit$learning, explicit$learning)
  expect_equal(fit$diagnostics, explicit$diagnostics)
  expect_equal(fit$aggregate$params, explicit$aggregate$params)
  expect_equal(fit$n_obs, sum(transition_counts[rownames(transition_counts) != "aggregate", ]))
  expect_identical(as.character(fit$call[[1L]]), "fit_item_lca")
})

test_that("fit_item_lca preserves named item pairing and DK schema", {
  sim <- simulate_lca_dk(
    n = 1400, n_items = 2,
    gg = 0.25, gk = 0.15, gd = 0.10, kk = 0.15,
    dg = 0.10, dk = 0.10, dd = 0.15,
    gamma = c(0.20, 0.30), seed = 802
  )

  original <- fit_item_lca(sim$pre, sim$post)
  order <- c(2L, 1L)
  permuted <- fit_item_lca(
    sim$pre,
    sim$post[order]
  )

  expect_equal(original$params, permuted$params[, colnames(original$params)])
  expect_equal(original$learning, permuted$learning[names(original$learning)])
  expect_equal(rownames(original$params), c("gg", "gk", "gd", "kk", "dg", "dk", "dd", "gamma"))

  expect_error(fit_item_lca(sim$pre, sim$post, response_schema = "binary"), "empty")
})

test_that("DK summaries label both learning transitions accurately", {
  sim <- simulate_lca_dk(n = 1000, n_items = 1, seed = 804)
  fit <- fit_item_lca(sim$pre, sim$post)
  summary_output <- capture.output(summary(fit))

  expect_match(
    paste(summary_output, collapse = "\n"),
    "Learning Estimates \\(gk \\+ dk\\):"
  )
  expect_false(any(grepl("Learning Estimates \\(gk \\+ kd\\):", summary_output)))
})

test_that("fit_item_lca applies missingness and optimizer controls", {
  sim <- simulate_lca(n = 1200, n_items = 2, seed = 803)
  pre_test <- sim$pre
  post_test <- sim$post
  pre_test[1:100, 1] <- NA
  post_test[101:250, 2] <- NA

  fit <- fit_item_lca(pre_test, post_test, na_as = "missing")
  expected_pairs <- sum(!is.na(pre_test) & !is.na(post_test))
  expect_equal(fit$n_obs, expected_pairs)
  expect_error(
    fit_item_lca(
      pre_test,
      post_test,
      na_as = "missing",
      missing_action = "error"
    ),
    "missing"
  )
  expect_error(
    fit_item_lca(sim$pre, sim$post, control = list(outer.iter = 1L)),
    "did not converge"
  )
})
