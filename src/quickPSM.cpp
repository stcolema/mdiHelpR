# include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp ;

//' @title Quick PSM
//' @description Constructs a similarity matrix trying assuming that there are 
//' a small number of modes. This function is intended to be more clever in
//' calculating the PSM, relying upon the poor quality exploration of many MCMC
//' samplers. It probably is not a good idea in low dimensions.
//' 
//' @param partitions Matrix of label assignment for data across iterations.
//' 
//' @return A symmetric n x n matrix (for n rows in cluster record) describing 
//' the fraction of iterations for which each pairwise combination of points are
//' assigned the same label.
//' @export
// [[Rcpp::export]]
arma::mat quickPSM(arma::umat partitions, double tol = 0.001){
  
  // Initialise everything
  double entry = 0.0;                             // Hold current value
  arma::uword N = partitions.n_cols;              // Number of items clustered
  arma::uword R = partitions.n_rows;              // Number of MCMC samples taken
  arma::mat psm = arma::zeros<arma::mat>(N, N);    // Output similarity matrix 
  arma::umat id = arma::zeros<arma::umat>(N, N);  // Matrix recording identical items
  double threshold = (1 - tol) * R;
  
  // Assign diagonal values
  psm.diag() += (double)R;
  
  // Iterate over items calculating the unnomralised similarity between all 
  // pairs. The symmetry here means that we only need to compare each pair once.
  for (arma::uword i = 0; i < N - 1; i++){ 
    
    // If the current item is identical to any preceding item in how it is 
    // partitioned, than there is no need to calculate the specific entries
    if (sum(id.col(i)) > 0) {
      continue;
    }

    // Iterate over the remaining items
    for (arma::uword j = i + 1; j < N; j++){

      // The unnormailed similarity is the count of the number of samples for 
      // which the two items are within the same cluster
      entry = sum(partitions.col(i) == partitions.col(j));

      // If the score is equal to N (the maximum value) than the two items
      // will have identical entries for all entries in the PSM
      if (entry == threshold) {
        id(i, j) = 1;
        id(j, i) = 1;
      }
      
      // Update the PSM
      psm(i, j) = entry;
      psm(j, i) = entry;
      
    }
    
    if(sum(id.col(i) > 0)) {
      // Find which items are now known to be identical to the ith item
      arma::uvec id_members = find(id.col(i));

      // Update the PSM entries to be the same for all of these entries
      psm.each_col(id_members) = psm.col(i);
    }

  }
 
  
 // Normalise
 psm = psm / R;
 
 // Return the PSM
 return psm;
  
}