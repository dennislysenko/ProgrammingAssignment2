## Provides two functions that allow more efficient matrix
## solving through a cache that makes extensive use of R's
## lexical scoping rules.

## Makes a matrix which you can use as an argument to
## cacheSolve() in order to find its inverse with 
## built-in caching.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(matrix) {
        x <<- matrix
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Solves a given matrix object gotten from makeCacheMatrix.
## If it manages to pull the inverse (solution) from cache,
## will display a message that reads "getting cached data".

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
