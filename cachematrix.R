## Together these functions can calculate and cache the inverse of a matrix, which can be retrieved later without recalculation
## This first function is really a list of functions that set the values of the matrix and inverse, retrieve the matrix value, sets the inverse value, and retrieves the inverse value.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- null
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## This second function will check to see if there is a previous solution to the matrix stored in the above function. If so it will return that data. If not it will solve the inverse of the matrix and store it in the cache for future retrieval.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  dat <- x$get()
  i <- solve(dat) 
  x$setinverse(i)
  i
}