## R-programming assignment 2 code

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cache0 <- NULL
  getMatrix <- function() x
  setSolve <- function(solve0) cache0 <<- solve0
  getSolve <- function() cache0
  list(getMatrix = getMatrix,
       setSolve = setSolve,
       getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  m <- solve(x$getMatrix(), ...)
  x$setSolve(m)
  m
}
