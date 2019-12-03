#!/usr/bin/env Rscript

# find the similarity between the columns of a data.table
#' @title PSM entries
calcPSMEntries <- function(my_dt, n, p){
  # n <- nrow(my_dt)
  # p <- ncol(my_dt)
  
  # Declare output list (this will be a lower triangular matrix)
  out <- list()
  
  # Loop over columns
  for(i in 1:(p -  1)){
    out[[i]] <- compareColForPSM(i, my_dt)
  }
  
  # Find the proportion of similarity
  lapply(out, `/`, n)
  
}