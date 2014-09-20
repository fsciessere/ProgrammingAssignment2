## Matrix inversion is a costly computation, therefore, 
## the purpose of the function below is to minimize the
## number of inverse matrix calculation by caching in memory
## inverse matrixes, so that if same matrix is need to have
## its inverse calculated, this is not re-computed, but
## read simply from the cache (memory)

## This function is a wrapper to a matrix capable to 
## store its inverse alongside with the value of the
## matrix itself
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## Variable to store matrix inverse
  
  ## Sets the value of the underneath matrix
  set <- function(y) {
    x <<- y
    ## Resets inverse, since matrix has been changed
    inv <<- NULL 
  }
  
  ## Gets the value of the underneath matrix
  get <- function() x
  
  ## Sets the value of the inverse of matrix
  setinverse <- function(i) inv <<- i
  
  ## Gets the value of the inverse of matrix
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Solves the inverse of a matrixm, and caches the result
## This function expects as parameter a matrix created with
## makeCacheMatrix function
cacheSolve <- function(x, ...) {
  ## Checks if inverse of the matrix is in the cache
  inv <- x$getinverse()
  
  ## If in the cache return it
  if(!is.null(inv)) {
    message("getting cached inversed MATRIX")
    return(inv)
  }
  ## If not in the cache calculate the inverse ...
  data <- x$get()
  inv <- solve(data, ...)
  ## ... and stores in the cache
  x$setinverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
