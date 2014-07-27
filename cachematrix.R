## Function library for caching matrix values and their associated inverse

## Constructs an 'object' encapsulating a closure over the given value and an associated inverse value
## The 'object' is returned as a list containing three functions
##   * get        - Returns the current matrix value
##   * set        - Sets the current matrix value and resets the inverse
##   * getInverse - Gets the inverse of the matrix, calculating it if it has not been previously calculated

makeCacheMatrix <- function(value = matrix()) {
  inverse <- NULL
  set <- function(newValue) {
    value <<- newValue
    inverse <<- NULL
  }
  get <- function() value
  getInverse <- function(...) {
    if(is.null(inverse)) {
      message("calculating inverse of matrix")
      inverse <<- solve(value, ...)
    }
    inverse
  } 
  list(set = set, get = get, getInverse = getInverse)
}


## Retrieves the inverse of the matrix from the specified cacheMatrix value which
## must be in instance of object returned by makeCacheMatrix (visitor pattern)

cacheSolve <- function(cacheMatrix, ...) {
  ## Return a matrix that is the inverse of 'cacheMatrix'
  cacheMatrix$getInverse(...)
}
