#!/usr/bin/env Rscript

# Turn a triangular matrix with no diagonal into a symmetric matrix with 1's 
# along the diagonal
#' @title Construct symmetrix
#' @export
constructSymmetrix <- function(tiangular_mat, n, diag_value = 1){
  tiangular_mat + t(tiangular_mat) + (diag(n) * diag_value)
}