#!/usr/bin/env Rscript

#' @title Make Arandi Matrices
#' @importFrom mcclust arandi
#' @export
makeARIMatrices <- function(cl_alloc_list, n_datasets){
  n_iter <- nrow(cl_alloc_list[[1]])
  
  out_list <- list()
  base_matrix <- matrix(0, nrow = n_datasets, ncol = n_datasets)
  
  for(i in 1:n_iter){
    
    for(j in 1 : (n_datasets - 1)) {
      
      for(k in (j + 1) : n_datasets){
        
        cl_j <- cl_alloc_list[[j]][i,] %>% unlist()
        cl_k <- cl_alloc_list[[k]][i,] %>% unlist()
        
        base_matrix[j, k] <- mcclust::arandi(cl_j, cl_k)
        
      }
    }
    out_list[[i]] <- constructSymmetrix(base_matrix, n_datasets, diag_value = 0)
  }
  out_list
}