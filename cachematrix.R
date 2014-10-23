## Put comments here that give an overall description of what your
## functions do
# The following functions provide APIs to create an object which computes
# and returns the inverse of the input matrix. The benefit of the object
# created with these APIs is that the object caches the inverse of itself
# so as to minimize matrix inverse recomputations.


## Write a short comment describing this function
# makeCacheMatrix() is a contructor function which creates an object
# with a copy of the input argument and provides an 'Inverse' caching
# mechanism which saves time computing the matrix inverse each time the
# getinv() method is invoked.
# The object exposes a set of methods i.e. set, get, setinv and getinv
# via the list object that is returned as a return value.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# cacheSolve() is a utility function around the object created with
# makeCacheMatrix(). This function tests if the matrix inverse is NULL and
# sets the matrix inverse by using the solve() API.
# The matrix inverse is also returned as a return value of this function.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inverse = x$getinv()
	if (!is.null(inverse)) {
		message("getting cached inverse")
		return (inverse)
	}
	inverse = solve(x$get())
	x$setinv(inverse)
	inverse
}

