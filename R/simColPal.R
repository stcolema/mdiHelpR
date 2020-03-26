#!/usr/bin/env Rscript

#' @title data colour palette
#' @description Makes the colour palette I use for heatmaps of similarity 
#' matrices (such as posterior similarity matrices or consensus matrices). 
#' (white - blue).
#' @param n The number of shades in the palette (default is 100).
#' @return A colour palette appropriate for the `colours` parameter in `pheatmap`.
#' @importFrom grDevices colorRampPalette
#' @export
simColPal <- function(n){
  grDevices::colorRampPalette(c("white", "#146EB4"))(100)
}