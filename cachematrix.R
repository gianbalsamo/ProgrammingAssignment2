## This function caches NULL as the value of 'inverse'. Unless a matrix is cached as
## 'inverse' via the setInverse() function, the inverse of matrix 'x' must be obtained
## and cached via the cacheSolve() function. The present function returns a list of four 
## functions.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      setMatrix <- function(y) {
            x <<- y
            inverse <<- NULL
            inverse
            #  inverse <<- solve(x)
      }
      getMatrix <- function() x
      setInverse <- function(inv) inverse <<- inv
      getInverse <- function() inverse #<< - solve(x)
      list(setMatrix = setMatrix, getMatrix = getMatrix, 
           getInverse = getInverse, setInverse = setInverse)
}

## This function checks if the inverse of 'x' was already cached by the
## makeCacheMatrix() function (specifically, via the setInverse() function). 
## If it was, it retrieves it from the cache. If it wasn't, 
## it computes it, caches it via the makeCacheMatrix() function (specifically, via 
# x$setInverse(inv) function), and returns it.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverse <- x$getInverse()
      if(!is.null(inverse)) {
            message('getting cached Inverse')
            return(inverse)
      }
      data <- x$getMatrix()
      inverse <- solve(data,...)
      x$setInverse(inverse)
      inverse
}


