#!/usr/bin/env Rscript

#' @title Compare matrices
#' @description Creates a plot with the heatmaps of m_1 and m_2 side by side.
#' There is an option to order the matrices based on the ordering imposed upon
#' m_1 by hclust.
#' @param ... A number of matrices.
#' @param matrices If matrices are not given in ``...``, a list of matrices to
#' be heatmapped.
#' @param col_pal The colour palette for the heatmaps. If a single vector of
#' colours, then all heatmaps share the same colour palette, but the user may
#' give a vector of palettes corresponding to each matrix passed to the
#' function. If ``NULL``, ``,diHelpR::defineColPal`` is called.
#' @param breaks Similarly to ``col_pal``, a vector of breaks for the heatmaps,
#' either a single vector to be used by all or else a list corresponding to the
#' matrices passed to the function. Defaults to
#' ``mdiHelpR::defineDataBreaks(m1, col_pal)`` where ``m1`` is the first matrix
#' passed to the function.
#' @param col_names A logical instructing inclusion of column names in the
#' heatmaps (default is TRUE).
#' @param order_rows A logical instructing the function to order the rows of all
#' matrices based upon the ordering imposed by hclust on the first.
#' @param order_cols A logical instructing the function to order the columns all
#' matrices based upon the ordering imposed by hclust on the columns of the
#' first.
#' @param method The linkage method used in imposing the ordering of rows and
#' columns. Defaults to "complete".
#' @param distance The distance measure used in imposing the ordering of rows and
#' columns. Defaults to "euclidean".
#' @param show_rownames A logical instructing inclusion of row names in the
#' heatmaps (default is FALSE).
#' @param show_colnames A logical instructing inclusion of column names in the
#' heatmaps (default is FALSE).
#' @param collect_legend Instruction to hide legend on all heatmaps and gather
#' (assumed) common legend to the left of the plots.
#' @param title The title of the final grid of plots.
#' @importFrom cowplot plot_grid draw_grob
#' @importFrom pheatmap pheatmap
#' @importFrom patchwork plot_annotation
#' @export
compareMatricesGen <- function(...,
                               matrices = NULL,
                               col_pal = NULL,
                               breaks = NULL,
                               order_rows = T,
                               order_cols = T,
                               n_row = NULL,
                               n_col = NULL,
                               method = "complete",
                               distance = "euclidean",
                               show_rownames = F,
                               show_colnames = F,
                               collect_legend = T,
                               title = NULL) {

  # Pass ellipses to a list and save some frequently used aspects
  if (!is.null(matrices)) {
    matrices <- list(...)
  }
  n_matrices <- length(matrices)
  m1 <- matrices[[1]]

  if (n_matrices > 9) {
    warning("The number of matrices given is quite large and probably will be cramped.")
  }

  # If the number of rows and columns of heatmaps to print is not given,
  # choose sensible numbers, defaulting to a square grid
  if (is.null(n_row) & is.null(n_col)) {
    n_row <- floor(sqrt(n_matrices))
    n_col <- ceiling(sqrt(n_matrices))
  }

  if (is.null(n_row)) {
    n_row <- ceiling(n_matrices / n_col)
  }

  if (is.null(n_col)) {
    n_col <- ceiling(n_matrices / n_row)
  }

  # Re order the matrices to have a common row order
  if (order_rows) {
    row_order <- findOrder(m1, method = method, distance = distance)

    for (i in 1:n_matrices) {
      matrices[[i]] <- matrices[[i]][row_order, ]
    }
  }

  if (order_cols) {
    col_order <- findOrder(t(m1), method = method, distance = distance)

    for (i in 1:n_matrices) {
      matrices[[i]] <- matrices[[i]][, col_order]
    }
  }

  # If no colour palettes or breaks given, make some and each identical for each
  # matrix
  if (is.null(col_pal)) {
    col_pal <- dataColPal()
  }

  if (is.null(breaks)) {
    breaks <- defineDataBreaks(m1, col_pal)
  }

  if (!is.list(col_pal)) {
    col_pal <- rep(list(col_pal), n_matrices)
  }


  if (!is.list(breaks)) {
    breaks <- rep(list(breaks), n_matrices)
  }

  # List of gtables from pheatmap to use
  ph_list <- vector("list", n_matrices)

  for (i in 1:n_matrices) {
    ph_list[[i]] <- pheatmap::pheatmap(matrices[[i]],
      cluster_rows = F,
      cluster_cols = F,
      color = col_pal[[i]],
      breaks = breaks[[i]],
      show_rownames = show_rownames,
      show_colnames = show_colnames,
      silent = TRUE,
      legend = !collect_legend,
      annotation_row = NA,
      annotation_colours = NA
    )$gtable
  }

  # Draw the legend grob using the function from pheatmap
  legend_grob <- draw_legend(col_pal[[1]], breaks[[1]])

  # Add this to the list
  # ph_list[[n_matrices+1]] <- legend_grob

  # Plot a grid of heatmaps
  p_grid <- cowplot::plot_grid(plotlist = ph_list, nrow = n_row, ncol = n_col)

  # If only one legend is necessary
  if (collect_legend) {
    p_grid <- cowplot::plot_grid(p_grid, NULL, rel_widths = c(10, 1))
    p_grid <- p_grid + cowplot::draw_grob(legend_grob, x = 0.925, y = -0.25)
  }

  # Add a title using patchwork if given
  if (!is.null(title)) {
    p_grid <- p_grid + patchwork::plot_annotation(title = title)
  }

  # Return the grid of pheatmaps
  p_grid
}
