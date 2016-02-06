## Put comments here that give an overall description of what your
## functions do

##--------------------------------------------------------------
## 2016.02.06 WM
## makeCacheMatrix and cacheSolve functions together calculate the inverse of 
## an input matrix and 'caches' it. 
##--------------------------------------------------------------

## Write a short comment describing this function
##--------------------------------------------------------------
## 2016.02.06 WM
## creates list of functions and stores input matrix
##--------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Write a short comment describing this function
##--------------------------------------------------------------
## 2016.02.06 WM
## Calcs the inverse matrix of the matrix inputted into makeCacheMatrix
## However, if inverse is already stored then just returns that
##--------------------------------------------------------------

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m      
}
