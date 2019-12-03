#!/usr/bin/env Rscript

# Functions to save comparison of heatmaps

#' @title defineBreaks
#' @export
defineBreaks <- function(col_pal, lb = -1, ub = 1){
  palette_length <- length(col_pal)
  
  breaks <- c(
    seq(lb, 0, length.out = ceiling(palette_length / 2) + 1),
    seq(1 / palette_length, ub, length.out = floor(palette_length / 2))
  )
}