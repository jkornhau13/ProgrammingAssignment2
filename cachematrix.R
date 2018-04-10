## function to get and set information about a 
## matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)
}

## Input is a list of the major methods of makeCacheMatrix
## Output is the inverse of the cached matrix. If it wasn't
## already computed (and thus is NULL), it gets computed.
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    mat <- x$get()
    x$setinverse(solve(mat, ...))
    x$getinverse()
}