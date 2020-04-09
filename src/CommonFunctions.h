#ifndef MODSTRING_H
#define MODSTRING_H

#include <RcppArmadillo.h>

double calcItemSimilarity(arma::uword point, 
                          arma::uword comparison_point,
                          arma::umat cluster_record,
                          arma::uword num_iter
);

arma::mat createSimilarityMat(arma::umat cluster_record);

#endif
