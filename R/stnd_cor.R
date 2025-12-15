#' Standard Guessing Correction for Learning
#'
#' Estimate of learning adjusted with standard correction for guessing. Correction is based on number of options per question.
#' The function takes separate pre-test and post-test dataframes. Why do we need dataframes? To accomodate multiple items.
#' The items can carry NA (missing). Items must be in the same order in each dataframe. Assumes that respondents are posed same questions twice. 
#' The function also takes a \code{lucky} vector --- the chance of getting a correct answer if guessing randomly. Each entry is 1/(number of options).
#' The function also optionally takes a vector carrying names of the items. By default, the vector carrying adjusted learning estimates takes same 
#' item names as the pre_test items. However you can assign a vector of names separately via \code{item_names}.
#'
#' @param pre_test Required. data.frame carrying responses to pre-test questions.
#' @param pst_test Required. data.frame carrying responses to post-test questions.
#' @param lucky    Required. A vector. Each entry is 1/(number of options)
#' @param item_names Optional. A vector carrying item names.
#' 
#' @return   a list of three vectors, carrying pre-treatment corrected scores, post-treatment scores, and adjusted estimates of learning
#' @export
#' @examples
#' # Without DK
#' pre_test <- data.frame(item1 = c(1,0,0,1,0), item2 = c(1,NA,0,1,0)) 
#' pst_test <- pre_test + cbind(c(0,1,1,0,0), c(0,1,0,0,1))
#' lucky <- rep(.25, 2); stnd_cor(pre_test, pst_test, lucky)
#' # With DK
#' pre_test <- data.frame(item1 = c(1,0,0,1,0,'d',0), item2 = c(1,NA,0,1,0,'d','d')) 
#' pst_test <- data.frame(item1 = c(1,0,0,1,0,'d',1), item2 = c(1,NA,0,1,0,1,'d')) 
#' lucky <- rep(.25, 2); stnd_cor(pre_test, pst_test, lucky)

stnd_cor <- function(pre_test = NULL, pst_test = NULL, lucky = NULL,
                     item_names = NULL) {

  # Input validation using utilities
  validate_dataframe(pre_test, "pre_test")
  validate_dataframe(pst_test, "pst_test")
  validate_compatible_dataframes(pre_test, pst_test)
  validate_lucky_vector(lucky, length(pre_test))

  pre_test_cor  <- pst_test_cor <- stnd_cor <- NA

  pre_test_cor <- mapply(
                    function(x, y) {
                      sum(x == 1, na.rm = TRUE) -
                      sum(x == 0, na.rm = TRUE) / (1 / y - 1)
                    },
                    pre_test, lucky, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  pst_test_cor <- mapply(
                    function(x, y) {
                      sum(x == 1, na.rm = TRUE) -
                      sum(x == 0, na.rm = TRUE) / (1 / y - 1)
                    },
                    pst_test, lucky, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  stnd_cor     <- pst_test_cor - pre_test_cor

  # Names of the return vector
  if ( is.null(item_names)) {

    names(pre_test_cor) <- names(pst_test_cor) <- names(stnd_cor) <-
        names(pre_test)

  } else {
    names(pre_test_cor) <- names(pst_test_cor) <- names(stnd_cor) <- item_names
  }

  pre   <- pre_test_cor / nrow(pre_test)
  pst   <- pst_test_cor / nrow(pst_test)
  learn <- stnd_cor / nrow(pre_test)

  list(pre = pre, pst = pst, learn = learn)
}
