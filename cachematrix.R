
## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mat <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mat);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(mat, ...) {
  inverse <- mat$getinv()
  if(!is.null(inverse)) {
    message("Retrieving cached data...")
    return(inverse)
  }
  ## Computing the inverse of a square matrix can be done with the solve function in R. 
  ## If X is a square invertible matrix, then solve(X) returns its inverse.
  
  data <- mat$get()
  invserse <- solve(data, ...)
  mat$setinv(inverse)
  return(inverse)
}
