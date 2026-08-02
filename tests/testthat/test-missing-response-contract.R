test_that("NA defaults to the same observed category as d and DK", {
  pre_na <- c(1, NA, 0, 1)
  post_na <- c(1, 1, NA, NA)
  pre_dk <- c("1", "d", "0", "1")
  post_dk <- c("1", "1", "DK", "D")

  from_na <- transmat(pre_na, post_na)
  from_dk <- transmat(pre_dk, post_dk)

  expect_equal(from_na, from_dk)
  expect_length(from_na, 9L)
  expect_equal(
    from_na[c("x11", "xd1", "x0d", "x1d")],
    c(x11 = 1, xd1 = 1, x0d = 1, x1d = 1)
  )
})

test_that("structural missingness omits only incomplete pairs", {
  pre <- c(1, NA, 0, "d")
  post <- c(1, 1, NA, 1)

  result <- transmat(pre, post, na_as = "missing")

  expect_length(result, 9L)
  expect_equal(sum(result), 2)
  expect_equal(unname(result["x11"]), 1)
  expect_equal(unname(result["xd1"]), 1)
})

test_that("structural missingness can be rejected", {
  expect_error(
    transmat(
      c(1, NA), c(1, 0),
      na_as = "missing", missing_action = "error"
    ),
    "Structural missing"
  )
  expect_error(
    multi_transmat(
      data.frame(i = c(1, NA)),
      data.frame(i = c(1, 0)),
      na_as = "missing", missing_action = "error"
    ),
    "Structural missing"
  )
})

test_that("invalid response codes are rejected centrally", {
  expect_error(
    transmat(c(1, "skipped"), c(1, 0)),
    "Responses must be coded"
  )
  expect_error(
    stnd_cor(
      data.frame(i = c(1, "skipped")),
      data.frame(i = c(1, 0)),
      lucky = 0.25
    ),
    "Responses must be coded"
  )
  expect_error(
    estimate_logit_score(data.frame(i = c(1, "skipped"))),
    "Responses must be coded"
  )
})

test_that("mixed DK and binary items share a valid nine-cell schema", {
  pre <- data.frame(
    binary = c(0, 0, 1, 1),
    dk = c("0", "d", "1", "d")
  )
  post <- data.frame(
    binary = c(0, 1, 0, 1),
    dk = c("1", "1", "1", "d")
  )

  result <- multi_transmat(pre, post)

  expect_equal(dim(result), c(2L, 9L))
  expect_equal(rowSums(result), c(item1 = 4, item2 = 4))
  expect_equal(
    unname(result["item1", c("x0d", "x1d", "xd0", "xd1", "xdd")]),
    rep(0, 5)
  )
})

test_that("stnd_cor uses the declared response interpretation", {
  pre_na <- data.frame(i = c(1, 0, NA, 1))
  post_na <- data.frame(i = c(1, 1, 1, NA))
  pre_dk <- data.frame(i = c("1", "0", "d", "1"))
  post_dk <- data.frame(i = c("1", "1", "1", "d"))

  expect_equal(
    stnd_cor(pre_na, post_na, lucky = 0.25),
    stnd_cor(pre_dk, post_dk, lucky = 0.25)
  )

  omitted <- stnd_cor(
    pre_na, post_na,
    lucky = 0.25, na_as = "missing"
  )
  expect_equal(omitted$pre, c(i = (2 - 1 / 3) / 3))
  expect_equal(omitted$pst, c(i = 1))
  expect_equal(omitted$learn, c(i = ((2 - 0 / 3) - (1 - 1 / 3)) / 2))
})

test_that("group adjustment preserves DK and structural missingness", {
  pre_na <- data.frame(i = c(1, 0, NA, 1, 0))
  post_na <- data.frame(i = c(1, 1, 1, NA, 0))
  pre_dk <- data.frame(i = c("1", "0", "d", "1", "0"))
  post_dk <- data.frame(i = c("1", "1", "1", "d", "0"))

  expect_equal(
    group_adj(pre_na, post_na, gamma = 0.25),
    group_adj(pre_dk, post_dk, gamma = 0.25)
  )

  omitted <- group_adj(
    pre_na, post_na,
    gamma = 0.25, na_as = "missing"
  )
  expect_true(is.na(omitted$indiv$pre_adj[3, 1]))
  expect_true(is.na(omitted$indiv$pst_adj[4, 1]))
  expect_equal(
    omitted$learn,
    colMeans(
      omitted$indiv$pst_adj - omitted$indiv$pre_adj,
      na.rm = TRUE
    )
  )
})

test_that("individual perplexity uses the observed pair denominator", {
  params <- c(gg = 0.4, gk = 0.3, kk = 0.3, gamma = 0.25)
  pre <- data.frame(i1 = c(0, 1), i2 = c(NA, 0))
  post <- data.frame(i1 = c(0, 1), i2 = c(1, 1))
  probs <- cell_probs(params)

  expected_individual <- c(
    exp(-log(probs[1])),
    exp(-(log(probs[4]) + log(probs[2])) / 2)
  )

  expect_equal(
    perplexity_individuals(
      params, pre, post,
      per_individual = TRUE, na_as = "missing"
    ),
    expected_individual
  )
  expect_equal(
    perplexity_individuals(params, pre, post, na_as = "missing"),
    exp(-(log(probs[1]) + log(probs[4]) + log(probs[2])) / 3)
  )
  expect_error(
    perplexity_individuals(params, pre, post),
    "require a DK model"
  )
})

test_that("ability scoring distinguishes DK from structural missingness", {
  responses_na <- data.frame(i = c(1, NA, 0))
  responses_dk <- data.frame(i = c("1", "d", "0"))

  expect_equal(
    estimate_logit_score(responses_na),
    estimate_logit_score(responses_dk)
  )
  omitted <- estimate_logit_score(responses_na, na_as = "missing")
  expect_true(is.na(omitted[2]))
})

test_that("raw-response wrappers forward missingness arguments", {
  pre <- data.frame(i = c(1, NA, 0, 1))
  post <- data.frame(i = c(1, 1, 0, 1))
  fit <- structure(
    list(
      params = matrix(
        c(0.4, 0.3, 0.3, 0.25),
        ncol = 1,
        dimnames = list(c("gg", "gk", "kk", "gamma"), "item1")
      )
    ),
    class = "guess_fit"
  )

  calls <- list(
    function() {
      stnd_cor(
        pre, post, 0.25,
        na_as = "missing", missing_action = "error"
      )
    },
    function() {
      group_adj(
        pre, post, 0.25,
        na_as = "missing", missing_action = "error"
      )
    },
    function() {
      lca_adj(
        pre, post,
        na_as = "missing", missing_action = "error"
      )
    },
    function() {
      item_lca_fit(
        pre, post,
        na_as = "missing", missing_action = "error"
      )
    },
    function() {
      lca_se(
        pre, post,
        n_resamples = 2,
        na_as = "missing", missing_action = "error"
      )
    },
    function() {
      fit_model(
        pre, post,
        g = 0.25, est_param = c(0.4, 0.3, 0.3),
        na_as = "missing", missing_action = "error"
      )
    },
    function() {
      perplexity_individuals(
        fit, pre, post,
        na_as = "missing", missing_action = "error"
      )
    },
    function() {
      person_item_lca_fit(
        pre, post,
        na_as = "missing", missing_action = "error"
      )
    },
    function() {
      estimate_logit_score(
        pre,
        na_as = "missing", missing_action = "error"
      )
    },
    function() {
      cross_sectional_learning(
        pre, post,
        na_as = "missing", missing_action = "error"
      )
    },
    function() {
      cross_sectional_learning_score(
        pre, post,
        na_as = "missing", missing_action = "error"
      )
    },
    function() {
      cv_individuals(
        pre, post,
        k = 2,
        na_as = "missing", missing_action = "error"
      )
    }
  )

  for (call in calls) {
    expect_error(call(), "Structural missing")
  }
})

test_that("zero-probability cells only matter when observed", {
  params <- c(gg = 0, gk = 0, kk = 1, gamma = 0.5)

  expect_equal(log_likelihood(params, c(0, 0, 0, 10)), 0)
  expect_equal(guess_lik(params, data = c(0, 0, 0, 10)), 0)
  expect_equal(log_likelihood(params, c(1, 0, 0, 9)), -Inf)
  expect_equal(guess_lik(params, data = c(1, 0, 0, 9)), Inf)
})

test_that("NA-coded DK data recover DK learning", {
  sim <- simulate_lca_dk(
    n = 8000, n_items = 1,
    gg = 0.25, gk = 0.15, gd = 0.10, kk = 0.15,
    dg = 0.10, dk = 0.10, dd = 0.15, gamma = 0.25,
    seed = 842
  )
  pre_na <- sim$pre
  post_na <- sim$post
  pre_na[pre_na == "d"] <- NA
  post_na[post_na == "d"] <- NA

  expect_equal(
    multi_transmat(pre_na, post_na),
    multi_transmat(sim$pre, sim$post)
  )

  fit <- suppressWarnings(item_lca_fit(pre_na, post_na))
  expect_equal(unname(fit$learning[1]), 0.25, tolerance = 0.04)
})
