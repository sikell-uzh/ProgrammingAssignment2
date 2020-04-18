
## "overall description" Cache the inverse of a matrix calculation 
## in a pair of functions


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set_matrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get_matrix <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set_matrix = set_matrix,
       get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get_matrix()
  inv <- solve(data, ...)
  x$set_inverse(inv)
  
  inv
}

