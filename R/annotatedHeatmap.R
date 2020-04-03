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
                             my_breaks = defineDataBreaks(x, col_pal, mid_point = 0),
                             main = "gen_dataset",
                             ...) {

  # Create the annotation data.frame for the rows
  anno_row <- data.frame(Cluster = factor(paste("Cluster", cluster_IDs))) %>%
    magrittr::set_rownames(rownames(x))

  # The number of cololurs to use
  K <- length(unique(cluster_IDs))

  # Create the annotation colours
  ann_colours <- list(Cluster = viridis::viridis(K) %>%
                        magrittr::set_names(paste("Cluster", sort(unique(cluster_IDs))))
                      )

  # Create the heatmap
  ph <- pheatmap::pheatmap(x,
    color = col_pal,
    breaks = my_breaks,
    annotation_row = anno_row,
    annotation_colors = ann_colours,
    main = main,
    ...
  )

  ph
}
