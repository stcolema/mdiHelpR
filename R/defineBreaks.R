#!/usr/bin/env Rscript

#' @title defineBreaks
#' @export
#' 
#' @description Defines breaks as an input for ``pheatmap`` for a given colour 
#' palette (to be used as the ``color`` input in ``pheatmap``).
#' 
#' @param col_pal A colour palette (of at least 4 entries)
#' @param lb The lower bound for the breaks (i.e. the minimum value that will be 
#' shown in the heatmap).
#' @param ub The upper bound on displayed values in the heatmap.
#' @param mid_point The mid point on the values for the pheatmap (defaults to 
#' the mean of ``lb`` and ``ub``).
#' 
#' @example defineBreaks(colorRampPalette(c("#146EB4", "white", "#FF9900"))(100), lb = -1, ub = 1, mid_point = 0)
defineBreaks <- function(col_pal, lb = 0, ub = 1, mid_point = NULL){
  palette_length <- length(col_pal)
  if(is.null(mid_point)){
    if(lb < 0 & ub > 0){
      mid_point <- 0
    } else {
      mid_point <- 0.5 * (lb + ub)
    }
  }

  breaks <- c(
    seq(lb, mid_point, length.out = ceiling(palette_length / 2) + 1),
    seq(mid_point + 1 / palette_length, ub, length.out = floor(palette_length / 2))
  )
}
