

## makeCacheMatrix function creates a special "Matrix" with 4 functions:
## get, set, setinverse, getinverse
## These functions will be used to keep the the matrix cache updated
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve tries to get the inverse from the "matrix" cache
## If inverse is already in the cache returns its value and reports cache hit
## Otherwise executes solve function, saves the result in the cache
## And returns the matrix inverse
## Usage: Given a matrix m
## cm <- makeCacheMatrix(m)
## cacheSolve(cm)


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
