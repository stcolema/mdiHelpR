#!/usr/bin/env Rscript

#' @title Set my theme
#' @description Set the theme I personally use for slides and whatnot. This is
#' theme_bw with the headers in a facet_wrap() set to navy with white text.
#' @importFrom ggplot2 theme_bw theme element_text element_rect
#' @export
setMyTheme <- function() {
  ggplot2::theme_set(ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "#21677e")) +
    ggplot2::theme(strip.text = ggplot2::element_text(colour = "white")))
}
