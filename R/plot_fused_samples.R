#!/usr/bin/env Rscript

# Function to plot heatmaps for fused genes from permutations of datasets

# define_breaks <- function(col_pal, lb = -1, ub = 1){
#   palette_length <- length(col_pal)
#   
#   breaks <- c(
#     seq(lb, 0, length.out = ceiling(palette_length / 2) + 1),
#     seq(1 / palette_length, ub, length.out = floor(palette_length / 2))
#   )
# }

#' @importFrom dplyr select one_of
#' @importFrom grDevices colorRampPalette
#' @importFrom magrittr set_rownames set_names
#' @importFrom pheatmap pheatmap
#' @export
fused_gene_heatmaps <- function(expression_data_lst,
                                fused_probes_lst,
                                gene_id,
                                datasets,
                                file_path,
                                n_datasets,
                                plot_type = ".png",
                                probes_present_dt = NULL #,
                                # show_row_labels = F
                                ) {

  # Set up the directory to save too
  dir_name <- paste0(file_path, "Fusion_expression_data/")
  dir.create(dir_name, showWarnings = FALSE)

  col_pal_expr <-  grDevices::colorRampPalette(c("#146EB4", "white", "#FF9900"))(100)
  
  # The generic heatmap file name
  generic_ph_title <- paste0(dir_name, "heatmap_")
  
  # Do the various combinations of datasets
  for (i in 1:(n_datasets - 1)) {
    d_i <- datasets[[i]]
    expression_data_i <- expression_data_lst[[i]]
    
    # number of people in current expression data
    n_col_i <- ncol(expression_data_i)
    
    for (j in (i + 1):n_datasets) {
      d_j <- datasets[[j]]
      expression_data_j <- expression_data_lst[[j]]
      
      n_col_j <- ncol(expression_data_j)
      
      # If too many columns to read in current expression data, hide column 
      # labels in the heatmaps
      n_col <- n_col_i + n_col_j
      show_col_labels = TRUE
      if(n_col > 50){
        show_col_labels = FALSE
      }
      
      # Extract the indices for the "fused" and "unfused" genes
      fused_ind <- fused_probes_lst[[i]][[j]] # fused_non_zero_probes[[1]][[2]]
      non_fused_ind <- !fused_ind
      
      
      # Find a nice ordering of the columns. This is superfluous and inefficient
      # ph1 <- pheatmap(expression_data_i, cluster_rows = F)
      # ph2 <- pheatmap(expression_data_j, cluster_rows = F)
      #
      # col_order_1 <- ph1$tree_col$order
      # col_order_2 <- ph2$tree_col$order
      #
      # new_expression_data <- cbind(
      #   expression_data_i[, col_order_1],
      #   expression_data_j[, col_order_2]
      # ) %>%
      #   magrittr::set_rownames(gene_id)
      
      # We ignore the above and instead have data unordered
      col_order_1 <- colnames(expression_data_i) # to allow no edits and to uncomment above for seamless integration

      new_expression_data <- cbind(
        expression_data_i,
        expression_data_j
      ) %>%
        magrittr::set_rownames(gene_id)

      expr_min <- min(new_expression_data)
      expr_max <- max(new_expression_data)
      
      expr_breaks <- define_breaks(col_pal_expr, lb = expr_min, ub = expr_max) %>% unique()
      
      # Filename for fused genes heatmap
      fused_ph_file_name <- paste0(
        generic_ph_title,
        "fused_genes_",
        d_i,
        "_",
        d_j,
        plot_type
      )
      
      
      # To avoid awkwardness use the values to not have them present
      annotation_colors <- fused_annotation_data <- unfused_annotation_data <- NA
      
      # If provided with data on which genes are present, use it for annotation
      if(! is.null(probes_present_dt)){
      
        annotation_data <- probes_present_dt %>% 
          dplyr::select(dplyr::one_of(c(d_i, d_j))) %>% 
          as.data.frame() %>% 
          magrittr::set_rownames(row.names(new_expression_data))
          # magrittr::set_rownames(probes_present_dt$Gene_names)
        
        annotation_data[! annotation_data] <- "Empty"
        annotation_data[annotation_data == T] <- "Present"
        
        # Use [] to keep data.frame format
        # Convert from character to factor
        annotation_data[] <- annotation_data %>% 
          lapply(as.factor)
        
        row.names(annotation_data) <- probes_present_dt$Gene_names
        
        annotation_colors_sub <- c("Present" = "red", "Empty" = "blue")
        
        annotation_colors <- list(annotation_colors_sub, annotation_colors_sub) %>% 
          magrittr::set_names(c(d_i, d_j))
        
        fused_annotation_data <- annotation_data[fused_ind, ] %>% 
          magrittr::set_rownames(probes_present_dt$Gene_names[fused_ind])
        
        unfused_annotation_data <- annotation_data[non_fused_ind, ] %>% 
          magrittr::set_rownames(probes_present_dt$Gene_names[non_fused_ind])
      }
      
      # If more than 1 fused gene can attempt to cluster rows
      show_row_labels <- T
      if (sum(fused_ind) > 1) {
        
        
        if(sum(fused_ind) > 50){
          show_row_labels <- F
        }

        pheatmap::pheatmap(new_expression_data[fused_ind, ],
          cluster_cols = F,
          gaps_col = length(col_order_1),
          main = paste("Fused probes for", d_i, "and", d_j),
          filename = fused_ph_file_name,
          annotation_row = fused_annotation_data,
          annotation_colors = annotation_colors,
          color = col_pal_expr,
          breaks = expr_breaks,
          show_rownames = show_row_labels,
          show_colnames = show_col_labels
        )
      } else {
        # If exactly 1 fused gene can still heatmap but no clustering
        if (sum(fused_ind) == 1) {

          # We have to do some odd things we extract a single entry - types 
          # change and info is lost as a result
          rel_gene <- row.names(new_expression_data)[fused_ind]
          
          # As we select only a single point we have to redeclare the row name 
          # (R hates selecting a single row)
          row.names(fused_annotation_data) <- rel_gene
          
          curr_data <- new_expression_data[fused_ind, ] %>%
            t() %>%
            as.data.frame() %>% 
            magrittr::set_rownames(rel_gene)
          
          pheatmap::pheatmap(curr_data,
            cluster_rows = F,
            cluster_cols = F,
            gaps_col = length(col_order_1),
            main = paste("Fused probes for", d_i, "and", d_j),
            filename = fused_ph_file_name,
            annotation_row = fused_annotation_data,
            annotation_colors = annotation_colors,
            color = col_pal_expr,
            breaks = expr_breaks,
            show_rownames = show_row_labels,
            show_colnames = show_col_labels
          )
        }
      }

      # File name for the heatmap fo the expression data for the unfused genes
      unfused_ph_file_name <- paste0(
        generic_ph_title,
        "unfused_genes_",
        d_i,
        "_",
        d_j,
        plot_type
      )

      

      
      # If have n > 1 can cluster rows. If n >= 1 can heatmap.
      if (sum(non_fused_ind) > 1) {
        pheatmap::pheatmap(new_expression_data[non_fused_ind, ],
          cluster_cols = F,
          gaps_col = length(col_order_1),
          main = paste("Unfused probes for", d_i, "and", d_j),
          filename = unfused_ph_file_name,
          annotation_row = unfused_annotation_data,
          annotation_colors = annotation_colors,
          color = col_pal_expr,
          breaks = expr_breaks,
          show_rownames = show_row_labels,
          show_colnames = show_col_labels
        )
      } else {
        if (sum(non_fused_ind) == 1) {
          pheatmap::pheatmap(new_expression_data[non_fused_ind, ],
            cluster_cols = F,
            cluster_rows = F,
            gaps_col = length(col_order_1),
            main = paste("Unfused probes for", d_i, "and", d_j),
            filename = unfused_ph_file_name,
            annotation_row = unfused_annotation_data,
            annotation_colors = annotation_colors,
            color = col_pal_expr,
            breaks = expr_breaks,
            show_rownames = show_row_labels,
            show_colnames = show_col_labels
          )
        }
      }
    }
  }
}
