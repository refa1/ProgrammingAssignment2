#This script contains a pair of functions that can be used to create a special 
#object which stores a matrix and caches the inverse of it. 

#This function creates a special "matrix" object that can cache its inverse.
        makeCacheMatrix <- function(x = matrix()) {
        inverse_1 <- NULL
        set <- function(y) {
                x <<- y
                inverse_1 <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverse_1 <<- inverse
        getInverse <- function() inverse_1
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        }

        
#This function computes the inverse of the special "matrix" 
        cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
                inv_2 <- x$getInverse()
                if (!is.null(inv_2)) {
                        message("getting cached data")
                        return(inv_2)
                }
                matrix_1 <- x$get()
                inv_3 <- solve(matrix_1, ...)
                x$setInverse(inv_3)
                inv_3
        }
