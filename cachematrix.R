## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
## sets cacheMatrix to NULL as a placeholder for a future value:
  cacheMatrix <- NULL
  ## defines a function:
  setmatrix <- function(y) {
    ## to set the matrix, x, to a new matrix, y:
    x <<- y
    ## and resets cacheMatrix to NULL:
    cacheMatrix <<- NULL
}
## returns the matrix, x:
  getmatrix <- function() x
  ## get inverse of matrix and then store it in cacheMatrix:
  setinverse <- function(solve) cacheMatrix <<- inverse
  ## returns the inverse matrix, cacheMatrix:
  getinverse <- function() cacheMatrix
  ## returns the 'special matrix' containing all of the functions just defined:
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}
## cacheSolve: This function computes the inverse
## of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function.
## For example, if X is a square invertible matrix, solve(X) returns its inverse.
cacheSolve <- function(x, ...) {
  ## sets the inverse matrix, cacheMatrix, to the value in the 'special matrix':
  cacheMatrix <- x$getinverse()
  ## if the inverse matrix, cacheMatrix, is not null, get and return cached matrix:
  if(!is.null(cacheMatrix)) {
    message("getting cached data")
    return(cacheMatrix)
  }
  ## otherwise, get and then set the cached inverse matrix:
  data <- x$getmatrix()
  cacheMatrix <- solve(data, ...)
  x$setmatrix(cacheMatrix)
  cacheMatrix
}
