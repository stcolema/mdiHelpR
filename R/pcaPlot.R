#!/usr/bin/env Rscript

#' @title PCA plot
#' @description Plot the items loadings along the principal components.
#' @param x A matrix of the loadings of each item upon each component (the "x" 
#' part of the prcomp() output).
#' @param labels A vector of the labels associated with the rows of x.
#' @param n_comp The number of components to plot.
#' @return A ``ggplot2'' object.
#' @export
pcaPlot <- function(x, labels, n_comp = 10){
  
  plt_pca_data <- x %>% 
    as.data.frame(row.names = row.names(.)) %>% 
    tibble::add_column(Cluster = as.factor(labels)) %>% 
    tidyr::gather(key = "Component", value = "Loading", -Cluster, factor_key = T)
  
  p <- ggplot(plt_pca_data, aes(x = Component, y = Loading, colour = Cluster)) +
    geom_point() +
    scale_color_viridis_d() +
    xlim(paste0("PC", 1:n_comp))
  
  p
}