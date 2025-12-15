#' multi_transmat: transition matrix of all the items
#'
#' @title Creates a transition matrix for each item.
#' @description Needs an 'interleaved' dataframe (see interleave function). Pre-test item should be followed by corresponding post-item item etc. 
#' Don't knows must be coded as NA. Function handles items without don't know responses.
#' The function is used internally. It calls transmat.
#' @param pre_test Required. data.frame carrying responses to pre-test questions.
#' @param pst_test Required. data.frame carrying responses to post-test questions.
#' @param subgroup a Boolean vector identifying the subset. Default is NULL.
#' @param force9   Optional. There are cases where DK data doesn't have DK. But we need the entire matrix. By default it is FALSE.
#' @param agg      Optional. Boolean. Whether or not to add a row of aggregate transitions at the end of the matrix. Default is FALSE.
#' @return matrix with rows = total number of items + 1 (last row contains aggregate distribution across items)
#' number of columns = 4 when no don't know, and 9 when there is a don't know option
#' @export
#' @examples
#' pre_test <- data.frame(pre_item1 = c(1,0,0,1,0), pre_item2 = c(1,NA,0,1,0)) 
#' pst_test <- data.frame(pst_item1 = pre_test[,1] + c(0,1,1,0,0), 
#'              pst_item2 = pre_test[,2] + c(0,1,0,0,1))
#' multi_transmat(pre_test, pst_test)

multi_transmat <- function (pre_test = NULL, pst_test = NULL,
                            subgroup = NULL, force9 = FALSE, agg = FALSE) {

  # Input validation using utilities
  validate_dataframe(pre_test, "pre_test")
  validate_dataframe(pst_test, "pst_test")
  validate_compatible_dataframes(pre_test, pst_test)

  # Apply subgroup filter if provided
  if (!is.null(subgroup)) {
    if (length(subgroup) != nrow(pre_test)) {
      stop("subgroup must have the same length as number of rows in data frames.")
    }
    if (!is.logical(subgroup)) {
      stop("subgroup must be a logical vector.")
    }
    pre_test <- pre_test[subgroup, , drop = FALSE]
    pst_test <- pst_test[subgroup, , drop = FALSE]
  }

  # No. of items
  n_items <- length(pre_test)

  # Initialize results
  res <- list()

  # Get transition matrix for each item pair
  for (i in seq_len(n_items)) {

    # cat("\n Item", i, "\n")
    res[[i]] <- transmat(pre_test[, i], pst_test[, i], force9 = force9)
  }

  # Format results using utility function
  result_matrix <- format_transition_matrix(res, n_items, add_aggregate = agg)
  
  invisible(result_matrix)
}
