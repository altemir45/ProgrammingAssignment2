

## Coursera - R Programming - Assignment 2
## By Altemir Palmede Pedroso
## The main objective is to create a matrix and cache its inverse rather than read and calculate it.
## The computation cost of matrix inversion is usually high for big data. So, it is possible to
## have some benefit to caching the inverse of the matrix rather than to read and calculate it.
 
## The function below ("makeCacheMatrix") creates a special matrix that can cache its inverse.
 
makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
       set <- function(y) {
          x <<- y
          inv <<- NULL
       }
       get <- function() x
       setInverse <- function(inverse) inv <<- inverse
       getInverse <- function() inv
       list(set=set, get=get, setInverse= setInverse, getInverse=getInverse)
}
  
## The function below ("cacheSove") calculates the inverse of the previous matrix. If the inverse has
## already calculated, then the it retrieve the inverse from the cache.
 
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
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
