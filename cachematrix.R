## The two functions below allow to store an inverse of a matrix in an R function closure and retrieve it 
## from cache when the function call on the same matrix is repeated. If a new matrix is specified
## a new inverse will be calculated.

## Return a list of functions allowing to set and store the matrix x, but also to set and store    
## its to-be calculated inverse i.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) i <<- getSolve
  getSolve <- function() i
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  i <- x$getSolve()
  if(!is.null(i)) {
    message("getting cached data")
    i
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setSolve(i)
  i
}