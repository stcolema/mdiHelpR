#!/usr/bin/env Rscript

# Bind the list of vectors from compare_col_for_psm into a triangular matrix
# Add a column of 0s on the end
#' @title Construct PSM
#' @export
constructPSM <- function(list_sim, n = NULL){
  
  # Bind the entries of the list into a matrix (each entry forms a column)
  psm <- do.call(cbind, list_sim)
  
  if(is.null(n)) {
    n <- ncol(psm)
  }
  
  # Add an empty column
  psm <- cbind(psm, rep(0, n) )
  psm
  
}