## This file contains a pair of functions that cache the inverse of a matrix. 
## When calling the two functions, if the inverse of the given matrix is already calculated and cached, 
## it will get it from the cache directly and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the inverse matrix in the cache for further use.


## the makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## it returns a list containing a function to:
## set the matrix
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##  
## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve will retrieve the inverse from the cache directly and skips the computation. 
## Otherwise, it computes the inverse of the special "matrix" and sets the inverse matrix in the cache for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  inv
}
