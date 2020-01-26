## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

## makeCacheMatrix creates the cash for the matrix inversion. It consist of 4 main steps
## first we set inv to null and also we use  <<- to asign x a different value from the main environment
## we do the same with inv
## we define the 4 subfunctions to get and set the cash whenever we can

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## We compute the inverse using INV and the makeCacheMatrix above
## If we already have the inverse (this one is set with setInverse in cache solve)
## we will get it with inv <- x$getinverse that has stored inv

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}