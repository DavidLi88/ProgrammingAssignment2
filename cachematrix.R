## Sample run-time example included results
## > source("cachematrix.R")    load R program
## > a <- makeCacheMatrix()     create functions
## > a$set(matrix(1:4, 2, 2))   create matrix 
## > cacheSolve(a)              1st run create and return inverted matrix
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cacheSolve(a)              2nd and subsequent runs returns inverted matrix from cache                        
## getting cached data          
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


## The following functions to create a square invertible matrix,
## and make the inverse of the matrix available in the cache.

## makeCacheMatrix creates and returns a list of functions used by cacheSolve to get or set the inverted matrix in cache
makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL;
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setInverse <- function(new_x_inv) x_inv <<- new_x_inv;
  getInverse <- function() x_inv;
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve is to inverse the matrix created in makeCacheMatrix and store it in cache 
## if the inverted matrix does not exist in cache, otherwise will get it from cache and return it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getInverse()
  if(!is.null(x_inv)) {
    message("getting cached inversed matrix")
    return(x_inv)
  }
  data <- x$get()
  new_x_inv <- solve(data, ...)
  x$setInverse(new_x_inv)
  new_x_inv
}
