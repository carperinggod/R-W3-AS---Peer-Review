## These two functions below are written to help ease the process of matrix
## computation by cache the inverse of specific matrix


## makeCacheMatrix() is written to create special object (matrix), that is 
## really a list containing a function to
##	1. Set the value of the matrix
##	2. Get the value fo the matrix
##	3. Set the value of the inverse of the matrix
##	4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	k <- NULL
	set <- function(y) {
		x <<- y
		k <<- NULL
	}
	get <- function()x
	setinverse <- function(inverse)k <<- inverse
	getinverse <- function()k
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve is written to return the inverse of the matrix created with the above function. It first checks to 
## see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverseof the matrix and sets the value of the inverse in the cache
## via the setinverse function.

cacheSolve <- function(x, ...) {
	k <- x$getinverse()
	if(!is.null(k)) {
		message("getting cached data")
		return(k)
	}
	data <- x$get()
	k <- solve(data, ...)
	x$setinverse(k)
	k
}
