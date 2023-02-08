## Put comments here that give an overall description of what your
## functions do

## Answer to Assignment 2: Lexical Scoping


# Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix2 <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve)s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
##
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve2 <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}