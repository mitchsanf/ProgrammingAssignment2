## The following two functions provide the functionality for an augmented
#    matrix that can cache the value of its inverse. The matrix is really a set
#    of function calls and two local variables, but it functions as a matrix. If the
#    function cacheSolve is called on a special matrix, it will return the inverse of
#    the special matrix

## makeCacheMatrix is a collection of functions for a special
#   matrix that is able to cache the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # updates the matrix x, resets the inverse to NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set,
        get = get,
        setinverse= setinverse,
        getinverse = getinverse)
}

## cacheSolve returns the inverse of a matrix. If the inverse has never been
#    calculated, or if the matrix has been changed, it does the full calculation
#    and stores the result. Otherwise, it returns the cached value

cacheSolve <- function(x) {
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setinverse(inv)
    inv
}