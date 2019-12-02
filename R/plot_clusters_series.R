#!/usr/bin/env Rscript

# Function to save plot of the numbers of clusters present in each iteration 
# of MCMC for each dataset

#' @importFrom ggplot2 ggplot aes geom_point labs ggsave
#' @export
plot_clusters_present <- function(n_clust_list,
                                  datasets, 
                                  n_datasets, 
                                  start_index,
                                  eff_n_iter,
                                  thin,
                                  file_path,
                                  gen_main_title = "Number of clusters present per iteration",
                                  gen_save_name = NULL,
                                  plot_type = ".png"){
  
  
  # Create directory to save this output in
  loc_dir <- paste0(file_path, "Cluster_series_plots/")
  dir.create(loc_dir, showWarnings = FALSE)
  
  # If the user has supplied a generic save name, use this, otherwise just use
  # the dataset name
  if(is.null(gen_save_name)){
    save_names <- paste0(loc_dir, datasets, plot_type)
  } else{
    save_names <- paste0(loc_dir, datasets, ": ", gen_save_name, plot_type)
  }
  
  # Generate plot titles
  main_titles <- paste0(datasets, ": ", gen_main_title)
  
  # Loop over datasets saving a plot for each
  for(i in 1:n_datasets){
    
    # Find the current dataset
    curr_dataset <- datasets[[i]]
    
    # Use the current save name
    save_name <- save_names[[i]]

    # Extract the current plot title 
    main_title <- main_titles[[i]]
    
    # Make a data.frame for the data (as we use ggplot2)
    .clust_data <- data.frame("n_clust" = n_clust_list[[i]], "Iteration" = start_index:eff_n_iter * thin)
    
    # Plot the number of clusters against iteration number as scatter plot
    .clust_plot <- ggplot2::ggplot(data = .clust_data, ggplot2::aes(x = Iteration, y = n_clust)) +
      ggplot2::geom_point() +
      ggplot2::labs(
        main = main_title,
        x = "Iteration",
        y = "Number of clusters present"
      )
    
    # Save the plot
    ggplot2::ggsave(save_name, plot = .clust_plot)
    
  }
}