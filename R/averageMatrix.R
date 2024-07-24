#!/usr/bin/env Rscript

#' @title Average matrix
#' @export
averageMatrix <- function(matrix_list){
  matrix_list |> 
    simplify2array() |> 
    apply(1:2, mean)
}