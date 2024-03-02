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

# Function to calculate an inverse matrix
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

# Function to create the vector of equations values
values_matrix <- function(n) {
  outMatrix <- matrix(0, nrow = n, ncol = 1)

  for (i in 1:n) {
    prompt <- paste("Enter the value of the ", i," equation: ")
    item <- readline(prompt = prompt)
    
    outMatrix[i,1] <- as.numeric(item)
  }

  outMatrix
}

res <- readline(prompt = "Enter the n value for nxn matrix: ")
n <- as.numeric(res)

custom <- custom_matrix(n)
inverse <- inverse_matrix(n, custom)
values <- values_matrix(n)

solutions <- inverse %*% values
print("Matrix with solutions:")
print(solutions)