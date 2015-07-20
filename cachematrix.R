## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## It creates and returns a list of functions used by cacheSolve 
## to get or set the inverse of the matrix in cache

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    	set <- function(y) {
        	x <<- y
        	inv <<- NULL
    	}
    	get <- function() x
    	setinverse <- function(inverse) inv <<- inverse
    	getinverse <- function() inv
    	list(set = set, 
    		get = get, 
    		setinverse = setinverse, 
    		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache. This function assumes that the 
## matrix is always investible

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
	if(!is.null(inv)) {
		message("Getting Cached Matrix.")
	        return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
    	inv
}
