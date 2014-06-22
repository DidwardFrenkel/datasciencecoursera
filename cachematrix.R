## Creates a list containing setters and getters for the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinv <- function(inv) inverse <<- inv
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Calculates the inverse of the inverse in the list created with the above
## function, but checks to see if it has already been calculated.
## If so, it gets the inverse from the cache.
## Else calculates normally and sets the inverse via setinv.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinv(inv)
	inv
}
