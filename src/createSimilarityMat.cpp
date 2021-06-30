# include <RcppArmadillo.h>
# include "CommonFunctions.h"

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp ;

//' @title Create Similarity Matrix
//' @description Constructs a similarity matrix comparing all points clustering across the 
//' iterations.
//' 
//' @param cluster_record Matrix of label assignment for data across iterations.
//' 
//' @return A symmetric n x n matrix (for n rows in cluster record) describing 
//' the fraction of iterations for which each pairwise combination of points are
//' assigned the same label.
//' @export
// [[Rcpp::export]]
arma::mat createSimilarityMat(arma::umat cluster_record){
  
  double entry = 0.0;                           // Hold current value
  arma::uword N = cluster_record.n_cols;        // Number of items clustered
  arma::uword n_iter = cluster_record.n_rows;   // Number of MCMC samples taken
  arma::mat out = arma::ones<arma::mat>(N, N);  // Output similarity matrix 

  // Compare every entry to every other entry. As symmetric and diagonal is I
  // do not need to compare points with self and only need to calcualte (i, j) 
  // entry
  for (arma::uword i = 0; i < N - 1; i++){ 
    for (arma::uword j = i + 1; j < N; j++){
      entry = (double)sum(cluster_record.col(i) == cluster_record.col(j)) / n_iter ; // calcItemSimilarity(i, j, cluster_record, n_iter);
      out(i, j) = entry;
      out(j, i) = entry;
    }
  }
  return out;
}
