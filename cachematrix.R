## This file includes functions for calculating and caching the inversion of a 
## matrix.

## This function creates a matrix which can cache its own inverse.

makeCacheMatrix <- function(x = numeric()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## This function computes the inverse of a matrix or retrieves the cached 
## value if the matrix hasn't changed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)	
	print(m)
}
