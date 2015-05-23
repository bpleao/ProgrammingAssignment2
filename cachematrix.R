## Programming Assignment 2
## cached inverse for a matrix

## makeCacheMatrix creates a list of functions that define my matrix based on an 
## ordinary matrix input

makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) xInv <<- inv
    getinv <- function() xInv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve computes and stores the matrix inverse into the defined matrix 
## object (if it has not yet been computed)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
