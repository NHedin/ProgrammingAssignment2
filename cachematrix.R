## This function creates a matrix that can store it's content and cache it's inverse
## Using this function will save time when working with large quantities of data 

## The "makeCacheMatrix" creates a matrix that can cache it's inverse. It allows the user
## to "set" and "get" the value and inverse of the matrix. 
## The "<<-" allows "x" and "invMat" to be stored within the enclosing environment
## of the functions.

makeCacheMatrix <- function(x = matrix()) {
  invMat = NULL  
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invMat <<- inverse
  getInverse <- function() invMat
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The "cacheSolve" functions computes the inverse of the matrix created in "makeCacheMatrix"
## The function will retrieve the inverse from the cache if it is already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMat <- x$getInverse()
  if (!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  mat <- x$get()
  invMat <- solve(mat, ...)
  x$setInverse(invMat)
  invMat
}
