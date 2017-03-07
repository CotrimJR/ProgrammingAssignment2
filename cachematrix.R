## =====================================================
## Caching the Inverse of a Matrix
## =====================================================
## Functions:
##   makeCacheMatrix()  --> Create Cache Matrix
##   cacheSolve()       --> Compute Inverse Matrix
## Example of use:
##   mtest <- makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
##   cacheSolve(mtest)
## =====================================================

## -----------------------------------------------------
## This function creates a special "matrix" object that
## can cache its inverse.
## -----------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## -----------------------------------------------------
## This function computes the inverse of the special
## "matrix" returned by 'makeCacheMatrix'.
## -----------------------------------------------------
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()

  if ( det(data) == 0 ) {
    message("The matrix is singular, not invertible.")
  } else {
    m <- solve(data, ...)
  }
  x$setsolve(m)
  m   ## Return a matrix that is the inverse of 'x'
}
