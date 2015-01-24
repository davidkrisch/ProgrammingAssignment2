## The following functions invert matrices.  If the inverse of
## the matrix has already been solved, a cached solution
## is returned.

## Creates a "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Value of the inverse
    inv <- NULL
    
    # Set the matrix
    set <- function(y) {
        # Store the matrix in the "outer" x
        x <<- y
        inv <<- NULL
    }
    
    # Get the matrix
    get <- function() x
    
    # Set the inverse
    setinv <- function(i) inv <<- i
    
    # Get the inverse
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Compute the inverse of the "matrix" returned by 
## makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
