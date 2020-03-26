#!/usr/bin/env Rscript

#' @title PCA series plot
#' @description Plot the median loadings for each cluster along the principal
#' components as a series, with an option to include a ribbon describing the 
#' range from the first ot the third quartile around this.
#' @param x A matrix of the loadings of each item upon each component (the "x"
#' part of the prcomp() output).
#' @param labels A vector of the labels associated with the rows of x.
#' @param n_comp The number of components to plot.
#' @param include_area Logical indicating inclusion of [quartile_1, quartile_3]
#' about the median value of the clusters across components.
#' @param ribbon_alpha The alpha used on the ribbon geom describing the range 
#' about the median of the clusters.
#' @param ribbon_lty The linetype used on the ribbon geom describing the range 
#' about the median of the clusters.
#' @return A ``ggplot2'' plot.
#' @importFrom ggplot2 ggplot geom_line scale_color_viridis_d xlim aes
#' @importFrom dplyr group_by summarise
#' @export
pcaSeriesPlot <- function(x, labels,
                          alpha = 0.8,
                          lty = 3,
                          n_comp = 10,
                          include_area = F,
                          ribbon_alpha = 0.2,
                          ribbon_lty = 4) {
  p1 <- pcaPlot(x, labels = labels, n_comp = n_comp)


  sum_data <- p1$data %>%
    dplyr::group_by(Cluster, Component) %>%
    dplyr::summarise(
      Median_value = median(Loadings),
      q1 = quantile(Loadings, probs = 0.25),
      q3 = quantile(Loadings, probs = 0.75),
      q0 = quantile(Loadings, probs = 0),
      q4 = quantile(Loadings, probs = 1)
    )


  p2 <- p1 +
    ggplot2::geom_line(ggplot2::aes(group = Item), alpha = alpha, lty = lty) +
    ggplot2::geom_line(
      data = sum_data,
      ggplot2::aes(x = Component, y = Median_value, group = Cluster),
      size = 1
    )

  if (include_area) {
    p2 <- p2 +
      scale_fill_viridis_d() +
      geom_ribbon(
        data = sum_data,
        aes(
          x = Component,
          y = Median_value,
          ymin = q1,
          ymax = q3,
          group = Cluster,
          fill = Cluster
        ),
        alpha = ribbon_alpha,
        lty = ribbon_lty
      )
  }

  p2
}
