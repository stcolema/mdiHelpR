#!/usr/bin/env Rscript

# Plot and save the mass parameters in the tibble
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot aes geom_point labs ggsave
#' @importFrom magrittr set_colnames
#' @importFrom tibble rowid_to_column
#' @export
plot_mass_parameters <- function(my_tibble, thin = 1L, file_path = "./", plot_type = ".png") {

  # Create the directory to save to
  dir_name <- paste0(file_path, "Mass_parameter_plots/")
  dir.create(dir_name, showWarnings = FALSE)

  n_datasets <- nrow(my_tibble)
  out_plots <- list()

  # Iterate voer the tibble saving a trace plot of the mass parameter
  for (i in 1:n_datasets) {

    # We use ggplot2, so make a dataframe of the mass parameter and iterations
    curr_data <- my_tibble$mass_parameter[[i]] %>%
      as.data.frame() %>%
      magrittr::set_colnames("Mass_parameter") %>%
      tibble::rowid_to_column("Iteration") %>%
      dplyr::mutate("True_iteration" = Iteration * thin) # account for thinning

    # For naming
    curr_dataset <- my_tibble$dataset[[i]]

    # The plot title
    title <- paste0(curr_dataset, ": Mass parameter across iterations")

    # Plot
    out_plots[[i]] <- ggplot2::ggplot(data = curr_data, ggplot2::aes(x = True_iteration, y = Mass_parameter)) +
      ggplot2::geom_point() +
      ggplot2::labs(
        title = title,
        x = "Iteration",
        y = "Value"
      )

    # Save name
    save_name <- paste0(dir_name, curr_dataset, plot_type)

    # Save
    ggplot2::ggsave(save_name)
  }

  # out_plots
}
