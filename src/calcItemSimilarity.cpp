# include <RcppArmadillo.h>
# include "CommonFunctions.h"

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp ;

//' @title Calculate item similarity
//' @decription Compares how similar two points are with regards to their clustering across 
//' all iterations. Works for unsupervised methods (i.e. allows label flipping).
//' 
//' @param point The index row within cluster_record for the first object.
//' @param comparison_point The comparison row index from cluster_record.
//' @param cluster_record A matrix of combined membership column vectors where 
//' the ijth entry is the membership assigned to the ith person in the jth 
//' iteration of sampling.
//' @param num_iter The number of samples recorded.
//' 
//' @return A score between 0 and 1 of the fraction of iterations for which the 
//' objects denoted by the point and comparison_point rows are assigned the same
//' label.
double calcItemSimilarity(arma::uword point, 
                          arma::uword comparison_point,
                          arma::umat cluster_record,
                          arma::uword num_iter
) {
  
  // Declare objects
  double out = 0.0;
  arma::uvec ind_1(num_iter);
  arma::uvec ind_2(num_iter);
  arma::umat out_1(num_iter, 1);
  
  // The relvant cluster vectors
  ind_1 = cluster_record.col(point);
  ind_2 = cluster_record.col(comparison_point);
  
  // Compare vector of allocations element-wise
  out_1.col(0) = (ind_1 == ind_2);
  
  // Similarity is the sum of the above divided by the number of entries
  // Convert the sum to a double as otherwise is integer divison and does not 
  // work
  out = (double)arma::sum(out_1.col(0)) / (double)num_iter;
  
  return out;
}
