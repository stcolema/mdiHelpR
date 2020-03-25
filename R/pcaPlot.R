#!/usr/bin/env Rscript

#' @title PCA plot
#' @description Plot the items loadings along the principal components.
#' @param x A matrix of the loadings of each item upon each component (the "x" 
#' part of the prcomp() output).
#' @param labels A vector of the labels associated with the rows of x.
#' @param n_comp The number of components to plot.
#' @return A ``ggplot2'' object.
#' @importFrom ggplot2 ggplot geom_point scale_color_viridis_d xlim
#' @importFrom tibble add_column
#' @importFrom tidyr gather
#' @export
pcaPlot <- function(x, labels = NA, n_comp = 10){
  
  if(is.na(labels)){
    
    # Gather the data from wide to long format and convert to a data.frame ready 
    # for ggplot
    plt_pca_data <- x %>% 
      as.data.frame(row.names = row.names(.)) %>% 
      tidyr::gather(key = "Component", value = "Loadings", factor_key = T)
    
    # plot the data along the components
    p <- ggplot(plt_pca_data, aes(x = Component, y = Loading)) +
      geom_point() +
      xlim(paste0("PC", 1:n_comp))
    
  } else {
    
    # Gather the data from wide to long format and convert to a data.frame ready 
    # for ggplot and include the labels as a factor
    plt_pca_data <- x %>% 
      as.data.frame(row.names = row.names(.)) %>% 
      tibble::add_column(Cluster = as.factor(labels)) %>% 
      tidyr::gather(key = "Component", value = "Loadings", -Cluster, factor_key = T)
    
    # plot the data along the components colouring by the labels
    p <- ggplot(plt_pca_data, aes(x = Component, y = Loading, colour = Cluster)) +
      geom_point() +
      scale_color_viridis_d() +
      xlim(paste0("PC", 1:n_comp))
  }
  
  p
}