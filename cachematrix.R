## Second assignment of 'R Programming' course from Hopkins University, enrolled in Coursera.
## This script includes two functions that will allow us to have an special matrix in our R environment
## that will be useful when we have to deal with very large data. Since the native method matrix() has no
## check if an identical matrix is alredy created in our workspace, this functionality will do.

## This function allow us to create a 'special' cache matrix that we can use when we are dealing with very large data.

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


## This function allow us to manage that 'special' cache matrix created with makeCacheMatrix. If the matrix is alredy
## created, it won't be calculated again.

cacheSolve <- function(x, ...) {
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
