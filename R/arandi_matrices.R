#!/usr/bin/env Rscript

#' @title Arandi 4 apply
#' @importFrom mcclust arandi
#' @export
arandi4Apply <- function(my_vec, break_col, end_col){
  
  my_vec_ <- unlist(my_vec)
  
  mcclust::arandi(my_vec_[1:break_col], my_vec_[(break_col+1) : end_col])
  
}

#' @title Make Arandi Matrices
#' @importFrom mcclust arandi
#' @export
makeARandIMatrices <- function(cl_alloc_list, n_datasets){
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
    out_list[[i]] <- construct_symmetrix(base_matrix, n_datasets, diag_value = 0)
  }
  out_list
}

#' @title Average matrix
averageMatrix <- function(matrix_list){
  matrix_list %>% 
    simplify2array() %>% 
    apply(1:2, mean)
}