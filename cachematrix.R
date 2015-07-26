## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. The following two functions are used to cache the inverse of a matrix.


## This function returns a list of functions
## Its puspose is to store a martix and a cached value of the inverse of the  matrix. Functions are the following:
## * setMatrix      set the value of a matrix
## * getMatrix      get the value of a matrix
## * cacheInv       set the cahced value (inverse of the matrix)
## * getInv         get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  getMatrix <- function() {
    x
  }
  
  cacheInv <- function(solve) {
    cache <<- solve
  }
  
  getInv <- function(){
    cache
  }
  
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix, 
       cacheInv = cacheInv, 
       getInv = getInv)
  
}


## The following function calculates the inverse of a "special" matrix created with makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  inv <- x$getInv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$getMatrix()
  inv <- solve(data)
  x$cacheInv(inv)
  
  inv
  
  
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
  
  ## Return a matrix that is the inverse of 'x'
}