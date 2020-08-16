## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  in <- NULL
  set <- function(y){
    x <<- y
    in <<- NULL
  }
  get <- function() {x}
  set_inverse <- function(inverse) {in <<- inverse}
  get_inverse <- function() {in}
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}



## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  in <- x$get_inverse()
  if(!is.null(in)){
    message("Getting cached data")
    return(in)
  }
  data <- x$get()
  in <- solve(data, ...)
  x$set_inverse(in)
  in
}
