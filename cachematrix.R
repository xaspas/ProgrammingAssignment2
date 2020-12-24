

## makeCacheMatrix function creates a special "Matrix" with 4 functions:
## get, set, setinverse, getinverse
## These functions will be used to keep the the matrix cache updated
makeCacheMatrix <- function(x = matrix()) {
  
  #Variable initialization to store inverted matrix
  i <- NULL
  
  #Functions definition to stored and retrieve data
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- inv_matrix
  getinverse <- function() i
  
  #Function result is a list of functions
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
  
  #Try to get the inverse matrix from the cache
  inv_matrix <- x$getinverse()
  
  #If inverse matrix was a hit in the cache then return cache value and inform
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  
  #Otherwise get matrix data and solve the inverse
  data <- x$get()
  inv_matrix <- solve(data, ...)
  
  #Store the result in the cache for future use
  x$setinverse(i)
  
  #Return inverse matrix
  inv_matrix
}
