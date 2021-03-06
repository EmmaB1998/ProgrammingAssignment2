## This function takes a matrix and returns the inverse. The special thing about 
## these two codes are that the inverse matrix is stored in a cache. If no else
## data is inserted in the makeCacheMatrix, it will return the last calculated 
## value when we ask for the inverse matrix. It is very useful, if we for example
## have a matrix, that almost always have the same value or to avoid using 
## unneccesary computer power to re-calculate the inverse.
## functions do

makeCacheMatrix <- function(x = matrix()) {
  ## We initialize the object "m", so that we can use it later in the code.
  m <- NULL
  ## The set function links this environment to the parent environment. By 
  ## setting m to NULL once again we clear any previous value of m
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## This line "gets" x from the parent environment of makeCacheMatrix
  get <- function() x
  ## This line defines the function to execute as well as assigning the input
  ## value to m in the parent environment
  setinverse <- function(inverse) m <<- inverse
  ## lastly we define the result, so that it can be used for the next lines 
  ## of code
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function calculates the inverse of the matrix defined above in the
## makeCacheMatrix. If this has already been calculated an no changes has been 
## made it just retrieves the value already calculated.

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
