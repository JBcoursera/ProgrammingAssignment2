## The following pair of functions cache the inverse of a matrix,
## because matrix inversion is usually a costly computation and
## there are some benefit to caching the inverse of a matrix.

## This function creates a special “matrix” object that can cache its inverse.
## Important: the matrix supplied must be invertible.

makeCacheMatrix <- function(x = matrix()) {
    
    inverse_matrix <- NULL
    
    set <- function(new_x) {
        x <<- new_x
        inverse_matrix <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inverse_input) inverse_matrix <<- inverse_input
    
    getinv <- function() inverse_matrix
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes and returns the inverse of the special “matrix”
## returned by the above function makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    inverse_matrix <- x$getinv()
    
    if (!is.null(inverse_matrix)) {
        message("Getting cached data")
        return(inverse_matrix)
    }
    
    data_x <- x$get()
    
    inverse_matrix <- solve(data_x,...)
    
    x$setinv(inverse_matrix)
    
    inverse_matrix
}
