## The functions caches the inverse of a matrix, so next time if the same matrix inverse is required 
## it should not do the inverse again, instead return the cache copy.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	 inv <- NULL
	  set <- function(y) {
		x <<- y
		inv <<- NULL
	  }
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

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
