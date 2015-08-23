## Create cache for process

makeCacheMatrix <- function(x = matrix()) {
  ##Create placeholder
  m <- NULL
  
  ##Set "x" to "y," and reset container
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Return value of x
  get <- function() x
  
  ## Set m to inverse (of matrix) 
  setinverse <- function(inverse) m <<- inverse
  
  ## Return m, the inverse
  getinverse <- function() m
  
  ## Creat list containing all four above steps
  list(set = set, get = get, setinverse=setinverse, 
       getinverse=getinverse)
}

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  ## Fetch cached value into m
  m <- x$getinverse()
  
  ## If cache is not empty, return it
  if(!is.null(m)){
    message("getting cached data")
    return (m)
  }
  
  ## When cache is empty:
  data <- x$get() # Get the original matrix (from above)
  m <- solve(data, ...) # Calculate the inverse
  x$setinverse(m) # Store it in the cache
  m # Return the inverse
}