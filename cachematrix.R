## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # Cache for the inverse
        
        set <- function(y) {
                x <<- y
                inv <<- NULL  # Reset cache if matrix is updated
        }
        
        get <- function() x
        set_inverse <- function(inverse) inv <<- inverse
        get_inverse <- function() inv
        
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## This function computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse is already cached, it retrieves the cached value.
cacheSolve <- function(x, ...) {
        inv <- x$get_inverse()
        
        if(!is.null(inv)) {
                message("Getting cached inverse")
                return(inv)
        }
        
        data <- x$get()
        
        # Check if matrix is square before attempting inversion
        if (nrow(data) != ncol(data)) {
                stop("Error: The matrix must be square to compute the inverse.")
        }
        
        inv <- solve(data, ...)  # Compute inverse
        x$set_inverse(inv)  # Cache the inverse
        inv
}
