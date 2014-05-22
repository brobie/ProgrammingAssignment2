## These functions provide a way to create an inverted matrix, and everytime 
## thereafter return a cached version of the matrix until another
## matrix is inverted.

## This function provides access to set and get a cached version of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setsolve <- function(inMatrix) im <<- inMatrix
    getsolve <- function() im
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function returns an inverse of the matrix given to the makeCacheMatrix
## function.  If the matrix has already been inverted and cached, it will return
## the cached version, otherwise, it will invert and then cache the matrix.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
