#!/usr/bin/env Rscript

#' @title Show
#' @description Show the first few rows and columns of a dataset.
#' @param x data.frame of interest
#' @param row_1 the first row shown
#' @param row_n the last row shown
#' @param col_1 the first column shown
#' @param col_n the last column shown
#' @return ``x[row_1:row_n, col_1:col_n]``
#' @export
show <- function(x, row_1 = 1, row_n = 4, col_1 = 1, col_n = 4) {
  x[row_1:row_n, col_1:col_n]
}