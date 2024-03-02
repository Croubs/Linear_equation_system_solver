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

# Function to create a matrix with user provided values
custom_matrix <- function(n) {
  outMatrix <- matrix(0, nrow = n, ncol = n)

  for (i in 1:n) {
    for (j in 1:n) {
      prompt <- paste("Enter the [", i, ",", j, "] number: ")
      item <- readline(prompt = prompt)
      
      outMatrix[i,j] <- as.numeric(item)
    }
  }
  
  outMatrix
}

inverse_matrix <- function(n, custom_matrix) {
  identity <- identity_matrix(n)
  
  for (i in 1:n) {
    pivot <- custom_matrix[i,i]
    custom_matrix[i,] <- custom_matrix[i,] / pivot
    identity[i,] <- identity[i,] / pivot

    for (j in 1:n) {
      if (j != i) {
        item_in_pivot_column <- custom_matrix[j,i]

        custom_matrix[j,] <- custom_matrix[j,] - (custom_matrix[i,] * item_in_pivot_column)
        identity[j,] <- identity[j,] - (identity[i,] * item_in_pivot_column)
      }
    }
  }

  identity
}

