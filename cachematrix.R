##########################################################################
## Two functions that calculate Matrix Inverse                          ##
## and cache the repeated values in case the matrix remains un changed. ##
##########################################################################

# Function that constructs a matrix which chace its inverse.
makeCacheMatrix <- function(m = matrix()) {
  
  # Inverse property initialization.
  mat_inv <- NULL
  
  # Setting the matrix.
  set <- function( matrix ) {
    mat <<- matrix
    mat_inv <<- NULL
  }
  
  # Getting the matrix.
  get <- function() {
    
    # Return the matrix.
    mat
  }
  
  # Function that sets the matrix inverse.
  setInverse <- function(inverse) {
    mat_inv <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    mat_inv
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

############################################################
# Matrix Inverse Computation of function "makeCacheMatrix" #
# Checking the value of the inverse, if it is found;       #
# the chached value retrived back.                         #
############################################################
cacheSolve <- function(x, ...) {
  
  # Get the inverse of Matrix x.
  mat <- x$getInverse()
  
  # Return the value of the inverse if it is set.
  if( !is.null(mat)) {
    message("Data Get Cached.")
    return(mat)
  }
  
  # Getting the data from object.
  data <- x$get()
  
  # Inverse calculation by matrix multiplication.
  mat <- solve(data) %*% data
  
  # Set the inverse to "mat" object.
  x$setInverse(mat)
  
  # Return the resulted matrix.
  mat
}