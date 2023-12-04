#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace arma;
using namespace Rcpp;

//' Perform Principal Component Analysis (PCA)
//' @return A list that has the mean vector, orthogonal matrix, Factor scores,and Eigenvalues
//' @param X A numeric matrix data or a data frame representing the input data
//' @param k An integer specifying the number of principal components to retain
//' @examples
//' # Testing the function
//' test.dat <- matrix(rnorm(500), nrow=100, ncol=5)
//' result.Plus <- PCAPlus(test.dat, k = 2)
//' print(result.Plus)
//' @export
// [[Rcpp::export]]
 Rcpp::List PCAPlus(const arma::mat& X, int k) {
   int n = X.n_rows, p = X.n_cols;

   // Vector to store column means
   arma::vec colMeans(p);

   // Step 1: Data Preprocessing (Standardize the Data)
   arma::mat X_std = X;
   for (size_t j = 0; j < p; ++j) {
     arma::vec col = X_std.col(j);
     double mean = arma::mean(col);
     double stddev = arma::stddev(col);
     colMeans(j) = mean; // Store the mean of each column
     X_std.col(j) = (col - mean) / stddev;
   }

   // Step 2: Compute the Covariance Matrix
   arma::mat cov_matrix = arma::cov(X_std);

   // Step 3: Compute Eigenvectors and Eigenvalues
   arma::vec eigenvalues;
   arma::mat eigenvectors;
   arma::eig_sym(eigenvalues, eigenvectors, cov_matrix);

   // Step 4: Sort the Eigenvalues and Eigenvectors in descending order
   arma::uvec indices = sort_index(eigenvalues, "descend");
   eigenvalues = eigenvalues(indices);
   eigenvectors = eigenvectors.cols(indices);

   // Check if k is larger than the number of eigenvectors
   if (eigenvectors.n_cols < k) {
     Rcpp::warning("The number of principal components is greater than the number of eigenvectors.");
     k = eigenvectors.n_cols; // Adjust k to the maximum possible value
   }

   // Step 5: Select the Principal Components
   arma::mat principal_components = eigenvectors.cols(0, k - 1);

   // Step 6: Data Projection
   arma::mat projected_data = X_std * principal_components;

   return Rcpp::List::create(
     Rcpp::Named("MeanVector") = colMeans, // Return the vector of column means
     Rcpp::Named("OrthogonalMatrix") = principal_components,
     Rcpp::Named("FactorScores") = projected_data,
     Rcpp::Named("Eigenvalues") = eigenvalues.subvec(0, k - 1)
   );
 }
