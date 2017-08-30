
## The function makeCacheMatrix takes a matrix as input and stores the inverse of the input matrix 
##in a cache in it's own environment to facilitate faster computation of the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL 
  ##initializes the value of the matrix
  set <- function(y = matrix()) { 
    x <<- y
    z <<- NULL
  }
  ##assigns the input to the mtarix x
  get <- function() x
  ##retrieves x from parent environment
  setinverse <- function(solve) z <<- solve
  ##calculates the inverse of x
  getinverse <- function() 
  ##passes value to the function 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
 
}

## The function cacheSolve computes the inverse of the "special" matrix created by makeCacheMatrix,
## if the inverse already exixts, it returns the inverse from the cache.


cacheSolve <- function(x, ...) {
  z <- x$getinverse()
  ##retrieves inverse if it has already been computed
  if(!is.null(z)) {
    message("retrieving cached data")
    return(z)
  }
  ##checks if result is NULL, if not NULL, cached inverse is returned
  data <- x$get()
  z <- solve(data, ...)
  x$setinverse(z)
  z
  ##returns inverse from prarent environment
}

