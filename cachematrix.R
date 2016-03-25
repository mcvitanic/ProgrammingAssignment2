# Example usage:
# > mm  <- matrix(rnorm(9), nrow = 3)          // Create a matrix mm  3 x 3
# > cmm <- makeCacheMatrix(mm)                 // Create our special matrix
# > cmm$get()                                  // Return the matrix
# > cacheSolve(cmm)                            // Return the inverse
# > cacheSolve(cmm)                            // Call the 2nd time, so return the cached inverse


# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the matrix. Contains the following functions:
# 	setMatrix      sets the value of a matrix
# 	getMatrix      gets the value of a matrix
# 	cacheInverse   gets the cahced value (inverse of the matrix)
# 	getInverse     gets the cahced value (inverse of the matrix)
#

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
	
    get <- function() x

    setinv <- function(inverse) inv <<- inverse
	
    getinv <- function() inv

    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()

    # If the inverse is already calculated, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # The inverse is not yet calculated, so we need to calculate it
    data <- x$get()
    inv <- solve(data, ...)

    # Cache the inverse
    x$setinv(inv)

    # Return the inverse
    inv
}