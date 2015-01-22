## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     
     ## set the value of the matrix
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     
     ## get the value of the matrix
     get <- function() x
     
     ## set the value of the inverse of the matrix
     setInv <- function(solve) m <<- solve
     
     ## get the value of the inverse of the matrix
     getInv <- function() m
     
     ## create a list of everything
     list(set = set, get = get,
          setInv = setInv,
          getInv = getInv)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     ## Get the cached inverse of x and set it to m
     ## This could be null
     m <- x$getInv()
     
     ## If m is not null, use the cache
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     ## Otherwise, get the matrix, invert it and cache it
     data <- x$get()
     m <- solve(data, ...)
     x$setInv(m)
     
     ## return m
     m
}
