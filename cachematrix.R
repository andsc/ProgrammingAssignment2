## Put comments here that give an overall description of what your
## functions do

## This function returns a list containing four functions:
##      - one to set the matrix
##      - one to get the matrix
##      - one to set the inverse of the matrix
##      - one to get the inverse of the matrix (if it has been set in advance,
##                                              NULL otherwise)

makeCacheMatrix <- function(x = matrix()) {
	cachedInverse <- NULL
	set <- function(m) {
		x <<- m
		cachedInverse <<- NULL
	}
	get <- function() {
		x
	}
	setinverse <- function(inverse) {
		cachedInverse <<- inverse
	}
	getinverse <- function() {
		cachedInverse
	}
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the special 'matrix' created by
## the previous version, but using the cached value if present

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
        	message("getting cached data")
        	return(inverse)
        }
        m <- x$get()
        inverse <- solve(m, ...)
        x$setinverse(inverse)
        inverse
}
