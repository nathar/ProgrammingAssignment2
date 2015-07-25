## Put comments here that give an overall description of what your
## functions do

# There are two functions -- makeCacheMatrix and cacheSolve. Both ensures a matrix is stored and enables its inverse to be 
# computed efficiently through caching. If the original matrix changed, then the old inverse will be set invalid thus requiring 
# a new computation.

## Write a short comment describing this function

# makeCacheMatrix has got nested functions that allows 
# one to store and retrieve a matrix using set() and get() functions respectively. The set() will unset (i.e. make NULL)
# the inverse whenever the original matrix changed.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
# cacheSolve computes the inverse of a matrix. However, if the inverse 
# was already computed, it will return the already computed inverse matrix instead of new computations.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
