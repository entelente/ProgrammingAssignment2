## This code is part of the second assigment of R Programming Course on Coursera
## The code computes and stores the inverse of a matrix to avoid unnecessary recalculations.
## The cached value is refreshed on matrix change.

## Returns a matrix object that caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(newMatrix) {
    x <<- newMatrix
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve){inverse <<- solve} 
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Calculates the inverse of the matrix and updates the cache or returns the cached inverse
## if it is already is in the cache.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data,...)
  x$setInverse(inverse)
  inverse ## Return a matrix that is the inverse of 'x'
}
