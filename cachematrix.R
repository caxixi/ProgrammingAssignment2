
## The two functions below accept a numric matrix as
## an input and returns the inverse matrix.

  

makeCacheMatrix <- function(x = matrix()) {
      
      ## This functioncreates a list vector 'list', which  
      ## contains four values: 1. Set the value for the matrix 
      ##2.Get the value if the matrix 3. Set the value of the 
      ## inverse matrix 4. Get the value of the inverted matrix.
      
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinvert <- function(invert) m <<- invert
      getinvert <- function() m
      list(set = set, get = get,
           setinvert = setinvert,
           getinvert = getinvert)
}



cacheSolve <- function(x, ...) {
      ##This function returns the inverted matrix of
      ## x: it first checks whether the inverted matrix 
      ## has already been calculated; if so, it gets the result 
      ## from the cache and skips the computation. Otherwise 
      ##, the inversion is newly calculated by the function.
      
      m <- x$getinvert()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinvert(m)
      m
}

