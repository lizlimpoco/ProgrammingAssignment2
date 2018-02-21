## Put comments here that give an overall description of what your
## functions do

## This function returns a list pertaining to the created matrix
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}


## This function checks if there is a cached inverse for the given matrix
## and returns this cached matrix; otherwise, it computes the inverse.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
