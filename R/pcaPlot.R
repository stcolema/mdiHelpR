#!/usr/bin/env Rscript

#' @title PCA plot
#' @description Plot the items loadings along the principal components.
#' @param x A matrix of the loadings of each item upon each component (the "x"
#' part of the prcomp() output).
#' @param labels A vector of the labels associated with the rows of x.
#' @param n_comp The number of components to plot.
#' @return A ``ggplot2'' object.
#' @importFrom ggplot2 ggplot geom_point scale_color_viridis_d xlim aes
#' @importFrom tibble add_column
#' @importFrom tidyr pivot_longer
#' @export
pcaPlot <- function(x, labels = NA, n_comp = 10) {
  if (all(is.na(labels)) & length(labels) == 1) {

    # Gather the data from wide to long format and convert to a data.frame ready
    # for ggplot
    plt_pca_data <- x %>%
      as.data.frame(row.names = row.names(.)) %>%
      tibble::add_column(Item = as.factor(row.names(.))) %>%
      tidyr::pivot_longer(-Item,
        names_to = "Component",
        values_to = "Loadings",
        names_ptypes = list(Component = factor()),
        values_ptypes = list(Loadings = numeric()),
      ) %>%
      suppressWarnings()


    # plot the data along the components
    p <- suppressWarnings(
      ggplot2::ggplot(plt_pca_data, ggplot2::aes(x = Component, y = Loadings)) +
      ggplot2::geom_point() +
      ggplot2::xlim(paste0("PC", 1:n_comp))
    )
    
  } else {

    # Gather the data from wide to long format and convert to a data.frame ready
    # for ggplot and include the labels as a factor
    plt_pca_data <- x %>%
      as.data.frame(row.names = row.names(.)) %>% 
      tibble::add_column(
        Cluster = as.factor(labels), 
        Item = as.factor(row.names(.))
      ) %>%
      tidyr::pivot_longer(-c(Cluster, Item),
        names_to = "Component",
        values_to = "Loadings",
        names_ptypes = list(Component = factor()),
        values_ptypes = list(Loadings = numeric())
      ) 

    # plot the data along the components colouring by the labels
    p <- ggplot2::ggplot(
        plt_pca_data, 
        ggplot2::aes(x = Component, y = Loadings, colour = Cluster)
      ) +
      ggplot2::geom_point() +
      ggplot2::scale_color_viridis_d() +
      ggplot2::xlim(paste0("PC", 1:n_comp))
  }

  p
}
