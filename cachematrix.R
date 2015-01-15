## Functions to extend the regular matrix class by allowing the matrix inverse
## to be cached for easy retrieval later on

## makeCacheMatrix takes a regular matrix and construct a new object that stores
## its inverse alongside. Methods are also provided to set/get values for the 
## matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) xinv <<- inv
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## cacheSolve efficiently returns the inverse of the extended matrix x. If the
## inverse of x is already known it returns it directly. If not it computes it
## and stores it in x for easy retrieval later on. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinv()
    if (!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    mat <- x$get()
    xinv <- solve(mat, ...)
    x$setinv(xinv)
    xinv
}
