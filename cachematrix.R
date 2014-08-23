## The functions makeCacheMatrix() and cacheSolve() work in tandem
# to create an object that can cache the inverse of a matrix.

# Create an object (in this case, a function together with an 
# environment) that can cache a matrix and its inverse. Take an
# invertible matrix and return setters and getters for both the
# argument and its inverse.  The cache environment is bound by
# the use of the super assignment operator in 'a function within
# a function'.
#
makeCacheMatrix <- function(x = matrix()) {
	matrixinverse <- NULL
	# There are two superassignments made within the setmatrix() 
    # function below, namely x <<- y and matrixinverse <<- NULL. 
    # Without the use of the superassignment operator, those 
    # assignments would not apply to the variables x and matrixinverse
    # out here, consequently neither x nor matrixinverse would cache 
    # properly. 
	setmatrix <- function(y) {
		x <<- y
		# Nulling out matrixinverse below guarantees that mutating 
		# the variable x through setmatrix() will force 
		# cacheSolve to re-compute a correct inverse.
		matrixinverse <<- NULL
	}
	getmatrix <- function() x
	setinverse <- function(inverse) matrixinverse <<- inverse
	getinverse <- function() matrixinverse
	list(setmatrix = setmatrix, 
			getmatrix = getmatrix,
			setinverse = setinverse,
			getinverse = getinverse)
}


# Take an object created by makeCacheMatrix and return the inverse of
# the underlying matrix which is held within the environment of that object.
# Checks first to see whether the object already holds a cached inverse,
# otherwise it computes the inverse outright.
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	# Were the original value of the matrix x underlying makeCacheMatrix
	# changed via the setmatrix() function, then the corresponding 
	# matrixinverse of makeCacheMatrix was nulled out. It means the 
	# null check here should be sufficient to detect both a null inverse
	# and a changed x.
	if(!is.null(inverse)) {
		message("getting cached inverse")
		return(inverse)
	}
	matrix_to_invert <- x$getmatrix()
	# It says "solve" below, which means "invert" in this setting.
	inverse <- solve(matrix_to_invert, ...)
	x$setinverse(inverse)
	inverse
}
