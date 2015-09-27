## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y 
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function calculates the inverse of the special matrix object created
## with the above function. It first checks if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise it calculates the inverse of the matrix and sets the 
## value of the inverse in the cache via the setinv function. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
