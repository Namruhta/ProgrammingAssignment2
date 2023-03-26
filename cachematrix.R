## Put comments here that give an overall description of what your
## functions do

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  def <- function(y) {
    x <<- y
    i <<- NULL
  }
  req <- function() x
  def_inv <- function(inv) i <<- inv
  req_inv <- function() i
  list(def = def,
       req = req,
       def_inv = def_inv,
       req_inv = req_inv)

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$req_inv()
  if (!is.null(i)) {
    return(i)
  }
  data <- x$req()
  i <- solve(data, ...)
  x$def_inv(i)
  i
        ## Return a matrix that is the inverse of 'x'
}

## EXAMPLE Matrix Output
M <- matrix(c(3, 9, -1, 4), 2)
MC <- makeCacheMatrix(M)
cacheSolve(MC)
