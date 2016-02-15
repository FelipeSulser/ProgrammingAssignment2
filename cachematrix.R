## Put comments here that give an overall description of what your
## functions do

## Function creates a special matrix object that stores the matrix and the inverse (if we execute the cachesolve method)
## Useful for caching data

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    # not the inv in this environment
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the matrix stored in the special matrix object.
# If inverse has already been calculated, this is, not null we retrieve it from cache (we do not re-compute it).

cacheSolve <- function(x, ...) {
  #set x's invert to the inverse matrix
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
