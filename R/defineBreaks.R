#!/usr/bin/env Rscript

# Functions to save comparison of heatmaps

#' @title defineBreaks
#' @export
defineBreaks <- function(col_pal, lb = -1, ub = 1){
  palette_length <- length(col_pal)
  
  mid_point <- mean(c(lb, ub))
  
  breaks <- c(
    seq(lb, mid_point, length.out = ceiling(palette_length / 2) + 1),
    seq(mid_point + 1 / palette_length, ub, length.out = floor(palette_length / 2))
  )
}
