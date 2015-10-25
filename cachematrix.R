## Programming assignment: Creating functions that caches the inverse of a matrix.
## The function only solves for the inverse if the cache does not exist.
## These functions assume that the input matrix is always invertible.

## makeCacheMatrix sets up get/set functions for both the matrix and also its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve first tries to get the cached inverse of the matrix,
## if the cache is invalid (i.e. null), then solves for it (and store the result in the cache).

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
