## A pair of functions that cache the inverse of a matrix
## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
	
## Initialize the inverse property
	i <- NULL

	## Method to set the matrix
	set <- function( matrix ) {
		m <<- matrix
		i <<- NULL
	}

	## Method the get the matrix
	get <- function() {
		## Return the matrix
		m
	}

	## Method to set the inverse of the matrix
	setInverse <- function(inverse) {
		i <<- inverse
	}

	## Method to get the inverse of the matrix
	getInverse <- function() {
		## Return the inverse property
		i
	}

	## Return a list of the methods
	list(set = set, get = get,
setInverse = setInverse,
getInverse = getInverse)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not


