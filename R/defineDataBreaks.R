#!/usr/bin/env Rscript

#' @title defineDataBreaks
#' @export
#' 
#' @description Defines breaks as an input for ``pheatmap`` for a given colour 
#' palette (to be used as the ``color`` input in ``pheatmap``) and dataset using 
#' the minimum and mximum values within ``x`` to define the range for the breaks.
#' 
#' @param x The dataset of interest.
#' @param col_pal A colour palette (of at least 4 entries)
#' @param mid_point The mid point on the values for the pheatmap (defaults to 
#' the mean of ``min(x)`` and ``max(x)``). Useful if there is a natural boundary
#' (e.g. 0).
defineDataBreaks <- function(x, col_pal, mid_point = NULL) {
  if (is.null(col_pal)) {
    col_pal <- dataColPal(n)
  }
  lb <- min(x, na.rm = TRUE)
  ub <- max(x, na.rm = TRUE)
  
  if(is.null(mid_point)){
    if(lb < 0 & ub > 0){
      mid_point <- 0
    } else {
      mid_point <- 0.5 * (lb + ub)
    }
  }
  
  defineBreaks(col_pal, lb = lb, ub = ub, mid_point = mid_point)
}