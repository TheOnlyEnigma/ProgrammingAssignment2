## Caching the Inverse of a Matrix:

## A makeCacheMatric function and cacheSolve Function, saved under the extension cachematrix.R, 
## that caches the inverse of a specified matrix:

## The function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse.
## This done by setting the value of the matrix, collecting the value of the matrix and then once
## this is collected the Inverse is set using the setInv, and returned using getInv.


## Fuctions: makeCacheMatrix, CacheSolve
## Objects: inv, set, get, setInv, getInv, mat


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInv <- function(inverse) inv <<- inverse
      getInv <- function() inv
      list(set = set, get = get,
           setInverse = setInv,
           getInverse = getInv)
      
}

## The function, cacheSolve, computes the inverse of the special "matrix" that is returned by
## by the makeCacheMatrix function. However, if the inverse has already been caculated, then this
## function will instead retrieve the inverse from the cache as the matrix has not been changed. 

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if (!is.null(inv)) {
            message("Collecting Cached Data...")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setInverse(inv)
      return(inv)
}
