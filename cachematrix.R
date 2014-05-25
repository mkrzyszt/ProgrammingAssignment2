## Function to calculate and then subsequently cache the inverse
## of a matrix. Function first check to see if matrix inverse exists
## in the cache before calculating


## Function which generates a list of functions for the inverse solver
## to check through. Ensures that inverse of matrix does not already 
## exist in the cache before computing it again.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Sets variable into parent environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Checks parent environment for existing variable
  get <- function() x
  #function that solves the inverse of hte matrix
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## First checks to see if inverse of inputted matrix exists. If not,
## calculates inverse

cacheSolve <- function(x, ...) {
  #Checks parent environment for inverse
  m <- x$getInverse()
  #If inverse exists, exit and return inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #Get the matrix to be computed, and solve
  data <- x$get()
  m <- solve(data, ...)
  #Sets solution into cache
  x$setInverse(m)
  m
}
