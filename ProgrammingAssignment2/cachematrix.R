## These functions let us wrap the matrices and their inverse,
## which are cached and only calculated once 

## makeCacheMatrix is a function of x matrix, which returns a cache matrix object. This cache object lets us 
## set and get the matrix and its inverse respectively via the
## set, get, setinverse, getinverse methods
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function of x , a cache matrix object, created from a matrix with the makeCacheMatrix function,
## which returns its inverse matrix.
## This function will calculate the inverse matrix and cache it the first time,
## and makes sure that subsequent calls of cacheSolve(x) return the cached inverse, also signalling it by
## a "getting cached data" console message
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
