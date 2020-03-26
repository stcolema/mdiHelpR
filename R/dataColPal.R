#!/usr/bin/env Rscript

#' @title data colour palette
#' @description Makes the colour palette I use for heatmaps of data.
#' (blue - white - orange).
#' @param n The number of shades in the palette (default is 100).
#' @return A colour palette appropriate for the `colours` parameter in `pheatmap`.
#' @importFrom grDevices colorRampPalette
#' @export
dataColPal <- function(n = 100){
  grDevices::colorRampPalette(c("#146EB4", "white", "#FF9900"))(n)
}