## The functions below calculate and store in the cache the inverse of 
## inversable, square vectors, and then recall the inverse vector for the 
## cache to save on performance


makeCacheMatrix <- function(x = matrix()) {
## This fucntion function modifies the makeVector function 
## to accept matrices as arguments, and to calculate the inverse rather 
## than the mean.
	m <- NULL
	setM <- function(y) {
		x <<- y
		m <<- NULL
	}
	getM <- function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	list(setM = setM, getM = getM,
		setInverse = setInverse,
		getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
## This function modifies the cachemean function in the example to solve 
## invertable square matrices.
## If the matrix created in 'makeCacheMatrix' is not square or not invertable,
## This function will return errors. 
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$getM()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}

