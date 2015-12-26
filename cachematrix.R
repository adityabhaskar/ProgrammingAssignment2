## makeCacheMatrix creates cache functions for a matrix, and returns them as a
## list 
##cacheSolve takes the list returned by makeCacheMatrix, and returns inverse of
##the matrix - from cache, else from calculation


# This function takes a matrix 'x' (or creates a 1x1 NA matrix) and returns a 
# list of functions to get & set the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Set inverse to NULL
  m <- NULL
  
  # Create a function 'set' that sets value of 'x' to 'y' and it's inverse to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Create a function 'get' that returns value of 'x'
  get <- function() x
  
  # Create a function 'setinverse' that sets value 'm' as inverse of 'x'
  setinverse <- function(inverse) m <<- inverse
  
  # Create a function 'getinverse' that returns the value of inverse of 'x'
  getinverse <- function() m
  
  # Return a list of functions to set and get a matrix and its inverse
  list(set = set, get = get,
   setinverse = setinverse,
   getinverse = getinverse)
}


# The function returns a pre-calculated inverse of a matrix, if it exists, else
# it calculates an inverse, caches and returns it
# 'x' is a list of get, set, getinverse and setinverse functions for the actual
# matrix whose inverse is being calculated
cacheSolve <- function(x, ...) {
  # Try to retrieve inverse of 'x'
  m <- x$getinverse()
  
  # If inverse exists in cache, i.e. is not null, return it and exit function
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If inverse doesn't exist in cache, get 'x'
  data <- x$get()
  
  # Calculate inverse
  m <- solve(data, ...)
  
  # Set inverse of x in cache for future retrieval
  x$setinverse(m)
  
  # Return inverse
  m
}

# NOTE: R's solve function can sometimes throw an error saying the matrix is
# singular. This can happen because R converts really small numbers to zero,
# making the determinant zero. One way to prevent this is to call cacheSolve
# with 'tol = 1e-30' as second parameter to prevent really small numbers from
# being considered as zero
