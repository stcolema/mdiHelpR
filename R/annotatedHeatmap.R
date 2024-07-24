#!/usr/bin/env Rscript

#' @title Annotated heatmap
#' @description Generate an annotated heatmap for output of
#' ``generateGaussianDataset`` or ``generateComplexDataset`` using the
#' ``pheatmap`` function.
#' @param x A matrix.
#' @param cluster_IDs A vector of the labels associated with the rows of x.
#' @param col_pal The colour palette to use in the heatmap.
#' @param my_breaks The breaks to use within the heatmap.
#' @param ... Other arguments to pass to ``pheatmap``.
#' @importFrom pheatmap pheatmap
#' @importFrom viridis viridis
#' @importFrom magrittr set_rownames set_names
#' @export
annotatedHeatmap <- function(x, cluster_IDs,
                             col_pal = colorRampPalette(c("#146EB4", "white", "#FF9900"))(100),
                             breaks = defineDataBreaks(x, col_pal),
                             cluster_names = paste("Cluster", cluster_IDs),
                             ...) {

  # Create the annotation data.frame for the rows
  anno_row <- data.frame(Cluster = factor(cluster_names)) |> 
    magrittr::set_rownames(rownames(x))

  # The number of cololurs to use
  K <- length(unique(cluster_IDs))

  # Create the annotation colours
  anno_colour_palette <- viridis::viridis(K)
  names(anno_colour_palette) <- sort(unique(cluster_names))
  ann_colours <- list(Cluster = anno_colour_palette)

  # Create the heatmap
  ph <- pheatmap::pheatmap(x,
    color = col_pal,
    breaks = breaks,
    annotation_row = anno_row,
    annotation_colors = ann_colours,
    ...
  )

  ph
}
