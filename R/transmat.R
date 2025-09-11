#' transmat: Cross-wave transition matrix
#'
#' @description Prints Cross-wave transition matrix and returns the vector behind the matrix.  
#' Missing values are treated as ignorance. Don't know responses need to be coded as 'd'.
#' @param pre_test_var Required. A vector carrying pre-test scores of a particular item. Only 
#' @param pst_test_var Required. A vector carrying post-test scores of a particular item
#' @param subgroup     Optional. A Boolean vector indicating rows of the relevant subset.  
#' @param force9       Optional. There are cases where DK data doesn't have DK. But we need the entire matrix. By default it is FALSE.
#' @return a numeric vector. 
#' Assume 1 denotes correct answer, 0 and NA incorrect, and d 'don't know.'
#' When there is no don't know option and no missing, the entries are: x00, x10, x01, x11
#' When there is a don't know option, the entries of the vector are: x00, x10, xd0, x01, x11, xd1, xd0, x1d, xdd
#' @export
#' @examples
#' pre_test_var <- c(1,0,0,1,0,1,0)
#' pst_test_var <- c(1,0,1,1,0,1,1)
#' transmat(pre_test_var, pst_test_var)
#' 
#' # With NAs
#' pre_test_var <- c(1,0,0,1,"d","d",0,1,NA)
#' pst_test_var <- c(1,NA,1,"d",1,0,1,1,"d") 
#' transmat(pre_test_var, pst_test_var)

transmat <- function(pre_test_var, pst_test_var, subgroup = NULL,
                    force9 = FALSE) {
  
  # Input validation
  if (is.null(pre_test_var) || is.null(pst_test_var)) {
    stop("Both pre_test_var and pst_test_var must be provided.")
  }
  
  if (length(pre_test_var) != length(pst_test_var)) {
    stop("pre_test_var and pst_test_var must have the same length.")
  }
  
  if (length(pre_test_var) == 0) {
    stop("Input vectors cannot be empty.")
  }

  # Apply subgroup filter if provided
  if (!is.null(subgroup)) {
    if (length(subgroup) != length(pre_test_var)) {
      stop("subgroup must have the same length as input vectors.")
    }
    if (!is.logical(subgroup)) {
      stop("subgroup must be a logical vector.")
    }
    pre_test_var <- pre_test_var[subgroup]
    pst_test_var <- pst_test_var[subgroup]
  }

  # Process and validate responses
  pre_test_clean <- nona(as.character(pre_test_var))
  pst_test_clean <- nona(as.character(pst_test_var))
  
  # Use validation utility
  validate_transition_values(pre_test_clean, pst_test_clean)
  
  # Count transitions using utility function
  transitions <- count_transitions(pre_test_clean, pst_test_clean)
  
  # Force DK matrix format if requested
  if (force9 && length(transitions) == 4) {
    # Convert 4-element to 9-element format with zeros for DK transitions
    dk_transitions <- rep(0, 9)
    names(dk_transitions) <- c("x00", "x01", "x0d", "x10", "x11", "x1d", "xd0", "xd1", "xdd")
    dk_transitions[c("x00", "x01", "x10", "x11")] <- transitions[c("x00", "x01", "x10", "x11")]
    transitions <- dk_transitions
  }
  
  return(invisible(transitions))
}
