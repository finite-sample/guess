#' Person Level Adjustment
#' 
#' @description Adjusts observed 1s based on item level parameters of the LCA model. Currently only takes data with Don't Know. And treats don't know responses as true confessions on ignorance.
#' If NAs are observed in the data, they are treating as acknowledgments of ignorance.
#' @param pre  pre data frame
#' @param pst  pst data frame
#' @return list of pre and post adjusted responses
#' @export
#' 
#' @examples
#' pre_test_var <- data.frame(pre = c(1, 0, 0, 1, "d", "d", 0, 1, NA))
#' pst_test_var <- data.frame(pst = c(1, NA, 1, "d", 1, 0, 1, 1, "d"))
#' lca_adj(pre_test_var, pst_test_var)

lca_adj <- function(pre = NULL, pst = NULL) {

  n <- nrow(pre)

  if ( sum(is.na(pre)) > 0 | sum(is.na(pst)) > 0 ) {
    warning("NAs will be converted to 0. MCAR is assumed.\n")
    pre <- as.data.frame(lapply(pre, function(x) {
                                       x[is.na(x)] <- 0
                                       x
                                      }))
    pst <- as.data.frame(lapply(pst, function(x) nona(x)))
  }

  transmatrix <- multi_transmat(pre, pst)

  lca_res     <- lca_cor(transmatrix)
  param_lca   <- lca_res$param.lca

  pk1 <-  n * param_lca["lkk", ] / sapply(pre, function(x) sum(x == 1))

  pk2 <-  n * (param_lca["lgk", ] +
               param_lca["lkk", ] +
               param_lca["lck", ]) / sapply(pst, function(x) sum(x == 1)
               )

  t1adj <- as.data.frame(mapply(function(x, y) ifelse(x == 1, y, x), pre, pk1))
  t2adj <- as.data.frame(mapply(function(x, y) ifelse(x == 1, y, x), pst, pk2))

  t1adj <-  sapply(t1adj, function(x) {
                             x[x == "d"] <- 0; x
                          })
  t2adj <-  sapply(t2adj, function(x) {
                            x[x == "d"] <- 0; x
                          })

  list(pre = t1adj, pst = t2adj)
}
