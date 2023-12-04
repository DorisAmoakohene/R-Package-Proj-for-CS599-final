# R-Package-Proj-for-CS599-final

# Installation
To install the CS599.pca.ADA package from the GitHub repository, follow these steps:

1. Open your R console or RStudio.
2. Make sure you have the remotes package installed. If not, install it by running
   
```
   install.packages("remotes")
```

4. Run the following command to install the package

```
remotes::install_github("DorisAmoakohene/R-Package-Proj-for-CS599-final")

```

# Example Usage
Here's an example of how to use the package:

```
# Load the package
library(CS599.pca.ADA)

#Example dataset
test.dat <- matrix(rnorm(500), nrow = 100, ncol = 5)

# Perform PCA
result.PCA <- PCA(test.dat, k = 2)

# Print the result
print(result.PCA)

```
