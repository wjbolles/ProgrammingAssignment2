## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Returns a list of 4 functions for returning precomputed inverse matrices:
# get, 
# set, 
# setinverse, 
# and getinverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
# Calculates the inverse of the matrix. If it has been already been computed it retrieves it using makeCacheMatrix
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

# using makeCacheMatrix & cacheSolve
testmat <- matrix(1:4, 2, 2)
test <- makeCacheMatrix(testmat)
cacheSolve(test)

# using solve
solve(testmat)