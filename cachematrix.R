## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
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


## Write a short comment describing this function
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
