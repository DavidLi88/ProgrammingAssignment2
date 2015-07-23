## The following functions to create a square invertible matrix,
## and make the inverse of the matrix available in the cache.

## makeCacheMatrix creates and returns a list of functions used by cacheSolve to get or set the inverte matrix in cache
makeCacheMatrix <- function(x = matrix()) {
  ## clear inverse matrix in cache
  x_inv <- NULL;
  
  set <- function(new_x) {
    x <<- new_x
    x_inv <<- NULL
  }
  get <- function() x
  
  ## funtion for sotring inverse matrix in cache
  setInverse <- function(new_x_inv) x_inv <<- new_x_inv;
  
  ## function for getting inverse matrix from cache
  getInverse <- function() x_inv;
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve is to inverse the matrix created in makeCacheMatrix and store it in cache 
## if the inverted matrix does not exist in cache, otherwise will get it from cache and return it.
cacheSolve <- function(x, ...) {
  ## Try to get a inverse matrix from cache, if it exists then return it.
  x_inv <- x$getInverse();
  if(!is.null(x_inv)) {
    message("getting cached inverse matrix");
    return(x_inv)
  }
  
  ## No inverse matrix in cache, then get the matrix and reverse it.
  data <- x$get();
  new_x_inv <- solve(data, ...);
  ## store inverse matrix in cache and return it.
  x$setInverse(new_x_inv);
  return(new_x_inv);
}

## Sample run-time example included results
## > source("cachematrix.R")      load R program
## > fn1 <- makeCacheMatrix()     create functions
## > fn1$set(matrix(1:4, 2, 2))   create matrix 
## > cacheSolve(fn1)              1st run create and return inverted matrix
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cacheSolve(fn1)              2nd and subsequent runs returns inverted matrix from cache                        
## getting cached data          
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
