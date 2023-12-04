#' Perform Principal Component Analysis (PCA)
#'
#'This function performs Principal Component Analysis (PCA) on the input data matrix X
#' and returns the projected data onto the selected principal components
#'
#' @param X A numeric matrix data or a data frame representing the input data
#' @param k An integer specifying the number of principal components to retain
#'
#' @return A list that has the mean vector, orthogonal matrix, Factor scores,and Eigenvalues
#' @export
#'
#' @examples #Example dataset
#' test.dat <- matrix(rnorm(500), nrow=100, ncol=5)
#' result.PCA <- PCA(test.dat, k = 2)
#' print(result.PCA)
#' PCA
#'
PCA <- function(X, k) {

  # Check if the number of components requested is greater than the number of available columns
  if (k > ncol(X)) {
    stop("Number of components requested exceeds the number of columns in the data matrix.")
  }

  # Calculate the mean vector
  meanVector <- colMeans(X)

  # Standardize the data
  standardizedData <- scale(X)

  # Compute the covariance matrix
  covarianceMatrix <- stats::cov(standardizedData)

  # Calculate eigenvalues and eigenvectors
  eigenValuesVectors <- eigen(covarianceMatrix)
  eigenvalues <- eigenValuesVectors$values
  eigenvectors <- eigenValuesVectors$vectors

  # Sort eigenvectors by descending eigenvalues
  order <- order(eigenvalues, decreasing = TRUE)
  sortedEigenvectors <- eigenvectors[, order]

  # Select the top 'k' eigenvectors
  principalComponents <- sortedEigenvectors[, 1:k]

  # Compute the factor scores
  factorScores <- standardizedData %*% principalComponents

  # Return the components
  list(
    MeanVector = meanVector,
    OrthogonalMatrix = principalComponents,
    FactorScores = factorScores, # the factor scores
    Eigenvalues = eigenvalues[order][1:k]
  )
}
