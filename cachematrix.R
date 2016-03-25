# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the matrix. Contains the following functions:
# 	setMatrix      sets the value of a matrix
# 	getMatrix      gets the value of a matrix
# 	cacheInverse   gets the cahced value (inverse of the matrix)
# 	getInverse     gets the cahced value (inverse of the matrix)
#

makeCacheMatrix <- function(x = numeric()) {
        cache <- NULL
		
        setMatrix <- function(newValue) {
                x <<- newValue
                cache <<- NULL
        }
		
        getMatrix <- function() {
                x
        }

        cacheInverse <- function(solve) {
                cache <<- solve
        }

        getInverse <- function() {
                cache
        }
        
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

# The following function calculates the inverse of a "special" matrix created with makeCacheMatrix
cacheSolve <- function(y, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- y$getInverse()
		
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
		
        # otherwise get the matrix, caclulate the inverse and store it in the cache
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        
        inverse
}
