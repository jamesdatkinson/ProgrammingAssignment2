## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly.
## Below are a pair of functions that cache the inverse of a matrix.

## Write a short comment describing this function

## This function creates an object that can cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  input <- function(y) {
    x <<- y
    inv <<- NULL
  }
  output <- function() x
  inInv <- function(inverse) inv <<- inverse
  outInv <- function() inv
  list(input = input,
       output = output,
       inInv = inInv,
       outInv = outInv)
}


## Write a short comment describing this function

## This function returns the inverse of the matrix
## created by makeCacheMatrix.
## If the inverse exists (and the matrix remains the same)
## it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$outInv()
  if (!is.null(inv)) {
    message("Retrieving cached matrix...")
    return(inv)
  }
  matrix <- x$output()
  inv <- solve(matrix, ...)
  x$inInv(inv)
  inv
}