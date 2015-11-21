## Assignment 2: the goal is to create a finction pair that cache the inverse of a matrix.

## The 'makeCacheMatrix' function creates a special matrix object, 
## which will cache its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The 'cacheSolve' function computes the inverse of the special matrix 
## returned from the above 'makeCacheMatrix'.

## If the inverse matrix has been calculated already, then this 'cacheSolve' 
## will output "getting cached data" & retireve the matrix from the cache.

cacheSolve <- function(x, ...) {
  cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
