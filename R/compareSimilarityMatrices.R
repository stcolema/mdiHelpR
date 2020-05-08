#!/usr/bin/env Rscript

#' @title Compare similarity matrices
#' @description Creates a plot with the heatmaps of matrices in a grid.
#' There is an option to order the matrices based on the ordering imposed upon
#' a matrix given. It is assumed all matrices have common dimenion and that
#' they share the same row and column names (althoug this does not directly
#' affect the outcome).
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
#' @param matrix_imposing_order An integer indicating which of the matrices
#' passed to the function should be used in deciding upon an order.
#' @param order_rows A logical instructing the function to order the rows of all
#' matrices based upon the ordering imposed by hclust on the first.
#' @param order_cols A logical instructing the function to order the columns all
#' matrices based upon the ordering imposed by hclust on the columns of the
#' first.
#' @param row_order A vector of indices of the order of rows in the matrices.
#' If ``NULL`` then row order is taken from the first matrix given.
#' @param order_cols A vector of indices of the order of columns in the matrices.
#' If ``NULL`` then column order is taken from the first matrix given.
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
#' @param lb The lower bound used in defining breaks in each heatmap (default is
#' 0).
#' @param ub The upper bound used in defining breaks in each heatmap (default is
#' 1).
#' @param mid_point The mid_point used in defining breaks in each heatmap
#' (default is 0.5).
#' @param title The title of the final grid of plots.
#' @importFrom cowplot plot_grid draw_grob
#' @importFrom pheatmap pheatmap
#' @importFrom patchwork plot_annotation
#' @export
compareSimilarityMatrices <- function(...,
                                      matrices = NULL,
                                      col_pal = NULL,
                                      breaks = NULL,
                                      matrix_imposing_order = 1,
                                      order_rows = T,
                                      order_cols = T,
                                      row_order = NULL,
                                      col_order = NULL,
                                      n_row = NULL,
                                      n_col = NULL,
                                      method = "complete",
                                      distance = "euclidean",
                                      show_rownames = F,
                                      show_colnames = F,
                                      collect_legend = T,
                                      title = NULL,
                                      lb = 0,
                                      ub = 1,
                                      mid_point = 0.5 * (lb + ub)) {

  # Pass ellipses to a list and save some frequently used aspects
  if (is.null(matrices)) {
    matrices <- list(...)
  }
  n_matrices <- length(matrices)
  m_star <- matrices[[matrix_imposing_order]]

  if (n_matrices > 9) {
    warning("The number of matrices given is quite large and probably will be cramped.")
  }

  # Re order the matrices to have a common row order
  if (order_rows) {
    if (is.null(row_order)) {
      row_order <- findOrder(m_star, method = method, distance = distance)
    }

    for (i in 1:n_matrices) {
      matrices[[i]] <- matrices[[i]][row_order, ]
    }
  }

  if (order_cols) {
    if (is.null(col_order)) {
      col_order <- findOrder(t(m_star), method = method, distance = distance)
    }

    for (i in 1:n_matrices) {
      matrices[[i]] <- matrices[[i]][, col_order]
    }
  }

  # If no colour palettes or breaks given, make some and each identical for each
  # matrix
  if (is.null(col_pal)) {
    col_pal <- simColPal()
  }

  if (is.null(breaks)) {
    breaks <- defineBreaks(m_star, col_pal,
      lb = lb,
      ub = ub,
      mid_point = mid_point
    )
  }

  if (!is.list(col_pal)) {
    col_pal <- rep(list(col_pal), n_matrices)
  }


  if (!is.list(breaks)) {
    breaks <- rep(list(breaks), n_matrices)
  }

  p_grid <- compareMatricesGen(
    matrices = matrices,
    col_pal = col_pal,
    breaks = breaks,
    order_rows = F,
    order_cols = F,
    n_row = n_row,
    n_col = n_col,
    method = method,
    distance = distance,
    show_rownames = show_rownames,
    show_colnames = show_colnames,
    collect_legend = collect_legend,
    title = title
  )

  p_grid
}
