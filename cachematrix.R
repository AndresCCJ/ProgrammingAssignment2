## The following sets of functions calculate and cache the results of matrix inverses
## if the inverse has been computed, it outputs the result without re-computing

## The following function creates a cache of the matrix whose inverse is being calculated

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## The following function either calculates the inverse of the matrix, if it has
## not been calculated yet, or computes it and stores it for future use

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
