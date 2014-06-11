## Use following functions to calculate and cache the inverse of a matrix.

## makeCacheMatrix: initializes the list of handler functions:
## set(matrix), get(), setinv(inverse), getinv()
## Use: initialize with v <- makeCacheMatrix(), load matrix with v$set(X)

makeCacheMatrix <- function() {
      m <- NULL
      x <- matrix()
      
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      get <- function() x
      
      setinv <- function(inv) m <<- inv
      
      getinv <- function() m
      
      ## Return: list of functions
      list(set = set,
           get = get,
           setinv = setinv,
           getinv = getinv)
}


## cacheSolve: returns the inverse of a matrix
## Only calculated if it is not cached already
## Use: cacheSolve(v) ## v initialized with makeCacheMatrix()

cacheSolve <- function(x) {
      
      ## Load inverse from cache
      m <- x$getinv()
      
      ## Retrieve if already cached
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      
      ## Load matrix if not cached already
      data <- x$get()
      
      ## Calculate inverse
      m <- solve(data)
      
      ## Store in cache
      x$setinv(m)
      
      m
}
