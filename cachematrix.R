## The functions below cache the inverse of a matrix.

## This function calculates the inverse of a matrix and caches it.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
          x <<- y
          inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}
## This function checks whether the inverse of the matrix has already 
## been calcuated and if it has it retreves it and if it hasn't it
## calculates it
cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if (!is.null(inv)) {
          message ("getting cached data")
          return(inv)
      }
      data <- x$get()
      inv <- solve(data,...)
      inv
  }
