## Here we'll create a "matrix object" which is able to cache its
## own inverse, once that inverse has been calculated, and we'll
## also create a function that finds (or recovers) the inverses
## of such objects.

## This first function creates the "matrix object" that caches its
## own inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This second function takes a "matrix object" (as created
## by the previous function) as input, and either recovers the
## inverse if it's been cached, or computes, stores, and returns
## the inverse otherwise.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinv(inv)
    inv
}
