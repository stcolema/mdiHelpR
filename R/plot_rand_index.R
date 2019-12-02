#!/usr/bin/env Rscript

# Function to scatter plot of adjusted rand index comparing final clustering to
# clustering at each iteration of MCMC
# compare_tibble$mdi_allocation

# mcclust::arandi behaves strangely with a named list (I thinl it compares names and entries)
# Therefore we create a function that can be called with lapply that unlsits the 
# vectors and applies arandi
#' @importFrom mcclust arandi
#' @export
unlist_arandi <- function(v1, v2){
  
  v1_ <- unlist(v1)
  v2_ <- unlist(v2)
  
  mcclust::arandi(v1_, v2_)
  
}

#' @importFrom ggplot2 ggplot geom_point aes labs ggsave
#' @export
plot_rand_index <- function(mdi_allocation_lst,
                            file_path,
                            dataset_names,
                            n_files,
                            n_datasets,
                            eff_n_iter,
                            burn = 0,
                            thin = 1) {
  generic_title <- "MDI: Adjusted Rand index for"

  start_index <- ceiling(burn / thin) + 1
  
  loc_dir <- paste0(file_path, "Adjusted_rand_index_plots/")
  dir.create(loc_dir, showWarnings = FALSE)

  # Define the plot type
  # plot_type <- ".pdf" # ".png" or ".pdf"

  # Create the lists to hold the dataset specific results
  rand <- list()
  rand_plots <- list()

  for (j in 1:n_files) {

    # Loop over the datasets
    for (i in 1:n_datasets) {
      # The current dataset name
      dataset <- dataset_names[[i]]
      
      # Append this to the generic title to create the specific title
      curr_title <- paste(generic_title, dataset)
      
      # Similarly for the name of the save file
      curr_save_file <- paste0(loc_dir, "rand_index_plot_", dataset, plot_type)
      
      # Create a vector of the adjusted rand index comparing the modal cluster
      # to the clustering at each iteration
      rand[[i + (j - 1) * n_files]] <- apply(
        mdi_allocation_lst[i + (j - 1) * n_files][[1]],
        # mdi_allocation[[i]],
        1,
        # mcclust::arandi,
        unlist_arandi,
        # mclust::adjustedRandIndex,
        mdi_allocation_lst[i][[1]][eff_n_iter - start_index, ]
        # compare_df[, i]
      )
      
      # As we use ggplot2, put this in a dataframe
      plot_data <- data.frame(Rand_index = rand[[i]], 
                              Iteration = (1:length(rand[[i]])) * thin
      )
      
      # Plot the Rand index against iteration number
      rand_plots[[i]] <- ggplot2::ggplot(data = plot_data) +
        ggplot2::geom_point(ggplot2::aes(x = Iteration, y = Rand_index)) +
        ggplot2::labs(
          title = curr_title,
          subtitle = "Comparing clustering in last iteration to clustering at each iteration",
          x = "Index",
          y = "Adjusted Rand Index"
        )

      if (save_plots) {
        # Save the plot
        ggplot2::ggsave(curr_save_file, plot = rand_plots[[i]])
      }
    }
  }
}
