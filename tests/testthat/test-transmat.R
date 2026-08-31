test_that("single- and multi-item transition counts agree", {
  pre_test <- c(1, 0, 0, 1, 0, 1, 0)
  post_test <- c(1, 0, 1, 1, 0, 1, 1)

  res <- count_transitions(pre_test, post_test)
  cor_ans <- c(x00 = 2, x01 = 2, x10 = 0, x11 = 3)

  expect_equal(sapply(res, as.numeric), cor_ans)

  pre_items <- data.frame(
    pre_item1 = c(1, 0, 0, 1, 0),
    pre_item2 = c(1, NA, 0, 1, 0)
  )

  post_items <- data.frame(
    pst_item1 = pre_items[, 1] + c(0, 1, 1, 0, 0),
    pst_item2 = pre_items[, 2] + c(0, 1, 0, 0, 1)
  )

  names(post_items) <- names(pre_items)
  res <- count_item_transitions(pre_items, post_items, na_as = "missing")
  cor_ans <- matrix(c(1, 2, 0, 2, 1, 1, 0, 2), byrow = TRUE, nrow = 2)
  rownames(cor_ans) <- names(pre_items)
  colnames(cor_ans) <- c("x00", "x01", "x10", "x11")
  expect_equal(res, cor_ans)
})

test_that("count_transitions has a stable, visible public contract", {
  expect_named(
    formals(count_transitions),
    c(
      "pre_test", "post_test", "...", "subgroup", "na_as",
      "missing_action"
    )
  )
  expect_visible(count_transitions(c(0, 1), c(1, 1)))
  expect_type(count_transitions(c(0, 1), c(1, 1)), "integer")
  expect_error(count_transitions(c(0, 1), c(1, 1), NULL), "must be empty")
  expect_false(exists("transmat", envir = asNamespace("guess"), inherits = FALSE))
})

test_that("count_transitions infers transition cells from normalized responses", {
  binary <- count_transitions(c(0, 1), c(1, 0))
  expect_named(binary, c("x00", "x01", "x10", "x11"))

  dk <- count_transitions(c(0, "d"), c(1, 0))
  expect_named(dk, c("x00", "x01", "x0d", "x10", "x11", "x1d", "xd0", "xd1", "xdd"))
  expect_equal(dk[c("x01", "xd0")], c(x01 = 1L, xd0 = 1L))
  expect_equal(
    unname(dk[c("x0d", "x10", "x1d", "xd1", "xdd")]),
    rep(0L, 5L)
  )
  expect_error(count_transitions(c(0, 1), c(1, 0), response_schema = "dk"), "empty")
})

test_that("count_transitions agrees with an exhaustive paired-response table", {
  pair_labels <- c("00", "01", "0d", "10", "11", "1d", "d0", "d1", "dd")
  expected <- seq_along(pair_labels)
  pre_test <- rep(substr(pair_labels, 1L, 1L), expected)
  post_test <- rep(substr(pair_labels, 2L, 2L), expected)

  result <- count_transitions(pre_test, post_test)

  expect_named(
    result,
    c("x00", "x01", "x0d", "x10", "x11", "x1d", "xd0", "xd1", "xdd")
  )
  expect_equal(unname(result), expected)
})

test_that("count_transitions rejects invalid subgroup selectors", {
  expect_error(
    count_transitions(c(0, 1), c(1, 0), subgroup = c(TRUE, NA)),
    "missing"
  )
  expect_error(
    count_transitions(c(0, 1), c(1, 0), subgroup = c(1, 0)),
    "logical"
  )
})
