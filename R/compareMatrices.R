#!/usr/bin/env Rscript

#' @title Compare matrices
#' @description Creates a plot with the heatmaps of m_1 and m_2 side by side.
#' There is an option to order the matrices based on the ordering imposed upon
#' m_1 by hclust.
#' @param m_1 A matrix.
#' @param m_2 A matrix (if ordering by columns or rows the corresponding
#' dimension must match that of m_1).
#' @param curr_title The title given to the final plot.
#' @param col_pal The colour palette for the heatmap of m_1 (corresponds to
#' the color parameter in pheatmap::pheatmap).
#' @param breaks The breaks for the colour palette of m_1 (corresponds to the
#' breaks parameter in pheatmap::pheatmap).
#' @param col_pal_2 The colour palette for m_2 (default is to match col_pal).
#' @param breaks_2 The breaks for the colour palette of m_2.
#' @param save_name The name to save the plot to (default is NA).
#' @param col_names A logical instructing inclusion of column names in the
#' heatmaps (default is TRUE).
#' @param order_rows A logical instructing the function to order the rows of m_1
#' and m_2 based upon the ordering imposed by hclust on m_1.
#' @param order_cols A logical instructing the function to order the columns of
#' m_1 and m_2 based upon the ordering imposed by hclust on t(m_1).
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @export
compareMatrices <- function(m_1, m_2,
                            curr_title = "compare_psms",
                            col_pal = c("#FFFFFF", grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 7, name = "Blues"))(100)),
                            breaks = defineBreaks(col_pal, lb = 0, ub = 1),
                            col_pal_2 = col_pal,
                            breask_2 = breaks,
                            save_name = NA,
                            col_names = T,
                            order_rows = T,
                            order_cols = T) {

  # Extract the order from the pheatmap
  row_order <- findOrder(m_1)
  col_order <- findOrder(t(m_1))

  # Re order the matrices to have a common row order
  if (order_rows) {
    m_1 <- m_1[row_order, ]
    m_2 <- m_2[row_order, ]
  }

  if (order_cols) {
    m_1 <- m_1[, col_order]
    m_2 <- m_2[, col_order]
  }

  if (!col_names) {
    colnames(m_1) <- NULL
    colnames(m_2) <- NULL
  }

  # Save the heatmaps of these to the same grid
  compareHeatmapPSMandData(m_1, m_2,
    save_name = save_name,
    main = curr_title,
    col_pal_sim = col_pal,
    sim_breaks = breaks,
    col_pal_expr = col_pal_2,
    expr_breaks = breask_2
  )
}
