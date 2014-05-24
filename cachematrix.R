## makeCacheMatrix and cacheSolve allow us to calculate and cache the matrix
## inverse, reducing computational time. To use, call:
##      a <- makeCacheMatrix(x)
##      cacheSolve(a)
## Subsequent calls of cacheSolve(a) will return the cached matrix inverse
## until a different a is created

## makeCacheMatrix creates a list of functions that allows us to cache the 
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(x) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve returns the cached matrix inverse if available in the cache. If
## the inverse is unavailable, the function calculates the inverse, returns it,
## and stores the inverse for later.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
