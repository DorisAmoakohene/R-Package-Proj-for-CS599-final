#' Perform Principal Component Analysis (PCA)
#'
#'This function performs Principal Component Analysis (PCA) on the input data matrix X
#' and returns the projected data onto the selected principal components
#'
#' @param X A numeric matrix data or a data frame representing the input data
#' @param k An integer specifying the number of principal components to retain
#'
#' @return A matrix containing the projected data onto the selected principal components
#' @export
#'
#' @examples #Example dataset
#' PCA.dat <- matrix(c(2, 4, 1, 3, 5, 7, 6, 8, 9), nrow = 3, ncol = 3, byrow = TRUE)
#' test.dat <- as.numeric(PCA.dat)
#' result.PCA <- PCA(test.dat, k = 2)
#' print(result.PCA)
#' PCA
#'

PCA<- function(X, k) {
  # Step 1: Data Preprocessing
  X_std <- scale(X)

  # Step 2: Compute the Covariance Matrix
  cov_matrix <- cov(X_std)

  # Step 3: Compute Eigenvectors and Eigenvalues
  eigen_result <- eigen(cov_matrix)
  eigenvalues <- eigen_result$values
  eigenvectors <- eigen_result$vectors

  # Step 4: Sort the Eigenvalues
  eigen_pairs <- data.frame(eigenvalues, eigenvectors)
  eigen_pairs <- eigen_pairs[order(eigen_pairs$eigenvalues, decreasing = TRUE), ]

  # Check if the number of columns in eigen_pairs is larger than k
  if (ncol(eigen_pairs) < k) {
    warning("The number of principal components is greater than the number of eigenvectors.")
    k <- ncol(eigen_pairs) # Adjust k to the maximum possible value
  }

  # Step 5: Select the Principal Components
  principal_components <- eigen_pairs[, 1:k]

  # Step 6: Data Projection
  projected_data <- as.matrix(X_std) %*%
    as.matrix(principal_components)


  return(projected_data)
}


