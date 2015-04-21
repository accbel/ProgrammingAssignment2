## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function will create a special object
## (a list with named elements as functions) to deal with matrix inverse
## and repeateable computations (using a cache region)
makeCacheMatrix <- function(base = matrix()) {
  ## create a cacheable matrix object and some associated sub-functions/methods
  
  ## define the cache 'cache'
  cache <- NULL
  
  set <- function(newMatrix) {
    ## assign the input matrix 'newMatrix' to the variable 'base' in the
    ## parent environment
    base <<- newMatrix
    cache <<- NULL ## re-initialize 'cache' in the parent environment to null
  }
  
  get <- function() {
    base ## return the matrix x
  }
  
  setInverse <- function(inverse) {
    ## set the cache 'cache' equal to the provided calculated inverse
    cache <<- inverse
  } 
  
  getInverse <- function() {
    cache ## return the cached inverse
  }
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function

# This function takes a cacheable matrix and calculates its inverse, storing the
# result in a cache region to avoid repeatable computations and waste of time
cacheSolve <- function(cacheMatrix, ...) {
  ## Return a matrix that is the inverse of 'cacheMatrix'
  
  ## Get inverse if any
  cache <- cacheMatrix$getInverse()
  
  ## If a cached result is present, return it immediately
  if(!is.null(cache)) {
    message("retrieving from cache...")
    return(cache)
  }
  
  ## If there is no cached result, do the computation and store it
  data <- cacheMatrix$get()
  cache <- solve(data, ...)
  cacheMatrix$setInverse(cache)
  cache
}