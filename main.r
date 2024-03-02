# Function to create an identity matrix
identity_matrix <- function(n) {
  outMatrix <- matrix(0, nrow = n, ncol = n)

  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        outMatrix[i,j] <- 1
      } else {
        outMatrix[i,j] <- 0
      }
    }
  }

  outMatrix
}

