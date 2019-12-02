#!/usr/bin/env Rscript

# Functions to create a Posterior similarity matrix

# find the similarity between the columns of a data.table
#' @title PSM entries
psm_entries <- function(my_dt, n, p){
  # n <- nrow(my_dt)
  # p <- ncol(my_dt)
  
  # Declare output list (this will be a lower triangular matrix)
  out <- list()
  
  # Loop over columns
  for(i in 1:(p -  1)){
    out[[i]] <- compare_col_for_psm(i, my_dt)
  }
  
  # Find the proportion of similarity
  lapply(out, `/`, n)
  
}

# Compare the ith column of a data.table to the [i+1,...,n]th columns
#' @title Compare col for PSM
compare_col_for_psm <- function(i, my_dt){
  
  # The column of interest
  x <- my_dt[, ..i] %>% c() %>% unlist()
  
  # The indices of the columns ot drop
  drop <- 1:i
  
  # A fill to ensure the output is the same lenght for every column (possibly
  # unnecessary)
  fill <- rep(0, i)
  
  # The number of entries exactly the same between the ith column and the 
  # [i+1,...,n]th columns
  y <- (x == my_dt[, -..drop]) %>% colSums()
  
  # Fill with 0s
  c(fill, y)
}

# Turn a triangular matrix with no diagonal into a symmetric matrix with 1's 
# along the diagonal
#' @title Construct symmetrix
#' @export
construct_symmetrix <- function(tiangular_mat, n, diag_value = 1){
  tiangular_mat + t(tiangular_mat) + (diag(n) * diag_value)
}

# Bind the list of vectors from compare_col_for_psm into a triangular matrix
# Add a column of 0s on the end
#' @title Construct PSM
#' @export
construct_psm <- function(list_sim, n = NULL){

  # Bind the entries of the list into a matrix (each entry forms a column)
  psm <- do.call(cbind, list_sim)
  
  if(is.null(n)) {
    n <- ncol(psm)
  }

  # Add an empty column
  psm <- cbind(psm, rep(0, n) )
  psm
  
}

# Wrapper function to find the PSM for a data.table
#' @title Make PSM
make_psm <- function(my_dt) {
  
  n <- nrow(my_dt)
  p <- ncol(my_dt)
  
  list_sim <- psm_entries(my_dt, n, p)
  psm <- construct_psm(list_sim, p)
  construct_symmetrix(psm, p)
}