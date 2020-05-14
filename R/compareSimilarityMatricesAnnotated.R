#!/usr/bin/env Rscript

#' @title Compare similarity matrices with annotation
#' @description Creates a plot with the heatmaps of matrices in a grid.
#' There is an option to order the matrices based on the ordering imposed upon
#' a matrix given. It is assumed all matrices have common dimenion and that
#' they share the same row and column names (althoug this does not directly
#' affect the outcome).
#' @param ... A number of matrices.
#' @param matrices If matrices are not given in ``...``, a list of matrices to
#' be heatmapped.
#' @param cluster_IDs The vector of factors used to annotate the matrices.
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
#' @param title The title of the final grid of plots.
#' @param lb The lower bound used in defining breaks in each heatmap (default is
#' 0).
#' @param ub The upper bound used in defining breaks in each heatmap (default is
#' 1).
#' @param mid_point The mid_point used in defining breaks in each heatmap
#' (default is 0.5).
#' @param fill A colour code used to fill the rectangle each heatmap is drawn 
#' upon. If ``NULL`` then no rectangle is drawn and there is no marked boundary
#' between heatmaps.
#' @importFrom cowplot plot_grid draw_grob
#' @importFrom pheatmap pheatmap
#' @importFrom patchwork plot_annotation
#' @export
compareSimilarityMatricesAnnotated <- function(...,
                                               matrices = NULL,
                                               cluster_IDs = NULL,
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
                                               mid_point = 0.5 * (lb + ub),
                                               fill = NULL) {

  # Pass ellipses to a list and save some frequently used aspects
  if (is.null(matrices)) {
    matrices <- list(...)
  }
  n_matrices <- length(matrices)
  m_star <- matrices[[matrix_imposing_order]]

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
    breaks <- defineBreaks(col_pal,
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

  # Pass ellipses to a list and save some frequently used aspects
  if (is.null(matrices)) {
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
    n_col <- ceiling(n_matrices / n_row)
  }

  if (is.null(n_row)) {
    n_row <- ceiling(n_matrices / n_col)
  }

  if (is.null(n_col)) {
    n_col <- ceiling(n_matrices / n_row)
  }

  # List of gtables from pheatmap to use
  ph_list <- ph_plt_lst <- vector("list", n_matrices)

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

    if (is.null(fill)) {
      ph_plt_lst[[i]] <- patchwork::wrap_elements(panel = ph_list[[i]]$grobs[[1]])
    } else {
      ph_plt_lst[[i]] <- patchwork::wrap_elements(
        panel = ph_list[[i]]$grobs[[1]],
        full = grid::rectGrob(gp = grid::gpar(fill = fill))
      )
    }
  }

  # Draw the legend grob using the function from pheatmap
  legend_grob <- draw_legend(col_pal[[1]], breaks[[1]])
  p_legend <- ggplot() +
    cowplot::draw_grob(legend_grob) +
    theme_minimal()

  # Plot a grid of heatmaps
  p_grid <- patchwork::wrap_plots(ph_plt_lst, nrow = n_row, ncol = n_col)

  if (!is.null(cluster_IDs)) {
    
    # impose the same ordering as used in heatmaps
    cluster_IDs <- cluster_IDs[row_order]
    
    # Match row and column names
    row_names <- row.names(m_star)
    if (is.null(row_names)) {
      row_names <- row.names(m_star) <- colnames(m_star) <- paste0("Person ", 1:nrow(m_star))
    }

    # Create the annotation data.frame for the rows
    anno_row <- data.frame(Cluster = factor(paste("Cluster", cluster_IDs))) %>%
      magrittr::set_rownames(row_names)

    # The number of cololurs to use
    K <- length(unique(cluster_IDs))

    # Create the annotation colours
    ann_colours <- list(Cluster = viridis::viridis(K) %>%
      magrittr::set_names(paste("Cluster", sort(unique(cluster_IDs)))))

    # Make the basic annotated heatmap to take our grobs from
    anno_plot <- pheatmap::pheatmap(m_star,
      annotation_row = anno_row,
      annotation_col = anno_row,
      annotation_colors = ann_colours,
      silent = T,
      cluster_rows = F,
      cluster_cols = F,
      show_colnames = F,
      show_rownames = F
    )$gtable

    # Construct the ggplot objects fit for patchwork
    anno_col_ggplot <- patchwork::wrap_elements(panel = anno_plot$grobs[[2]])
    anno_row_ggplot <- patchwork::wrap_elements(panel = anno_plot$grobs[[4]])

    # The label for the annotation
    label_col <- patchwork::wrap_elements(panel = anno_plot$grobs[[3]])
    label_row <- patchwork::wrap_elements(panel = anno_plot$grobs[[5]])

    # The annotation legend
    anno_legend <- patchwork::wrap_elements(panel = anno_plot$grobs[[6]])

    # Have an annotation row/column for each row/column in the plot grid
    anno_plot_row_grid <- anno_plot_col_grid <- list()
    for (i in 1:n_col) {
      anno_plot_row_grid[[i]] <- anno_col_ggplot
    }

    for (i in 1:n_row) {
      anno_plot_col_grid[[i]] <- anno_row_ggplot
    }

    # The annotation for the rows and columns respectively (names refer to plot position)
    anno_column <- patchwork::wrap_plots(anno_plot_col_grid, ncol = 1)
    anno_row <- patchwork::wrap_plots(anno_plot_row_grid, nrow = 1)

    # Legend for main plot and annotation
    legend_grid <- patchwork::wrap_plots(p_legend, anno_legend, nrow = 1)

    # Combine everything
    p_grid <- patchwork::wrap_plots(patchwork::plot_spacer(),
      anno_row,
      label_col,
      anno_column,
      p_grid,
      legend_grid,
      label_row,
      patchwork::plot_spacer(),
      patchwork::plot_spacer(),
      heights = c(1, 10, 2),
      widths = c(1, 10, 2)
    )
  } else {

    #
    p_grid <- patchwork::wrap_plots(p_grid,
      p_legend,
      patchwork::plot_spacer(),
      patchwork::plot_spacer(),
      heights = c(10, 1),
      widths = c(10, 1)
    )
  }

  # Add a title using patchwork if given
  if (!is.null(title)) {
    p_grid <- p_grid + patchwork::plot_annotation(title = title)
  }

  p_grid
}
