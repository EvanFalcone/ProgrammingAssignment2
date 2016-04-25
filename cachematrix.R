## Since matrix inversion can be a computationally costly operation, it
## can be beneficial to cache the inverse of a matrix instead of
## computing it iteratively/over and over.

## The following two functions create a special type of object that stores
## a matrix and caches its inverse, respectively.

## This first function creates a special "matrix" obj that can cache its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This second function is meant to compute the inverse of the particular
## "matrix" object from makeCacheMatrix above. If the inverse was already
## calculated and the matrix did not change, then the function returns
## the inverse from the cache data.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
