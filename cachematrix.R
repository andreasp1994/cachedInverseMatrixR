## Cached inverse of a matrix
## The following code snippet contains functions for creating matrices
## which have the capability of storing their inverse matrix in a cache.
## As a result, the costly operation of calculating the inverse matrix is only
## being calculated once and then fetched from the memory.

## makeCacheMatrix 
## This function creates a special matrix which is able to cache is inverse.
## In reality it is a list object which has getter and setter functions as elements.
## 	
##	parameters:
##	x - a matrix;
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		#save vars in new enviroment
		x <<- y 
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve
## This is a function which get a special matrix created by the above function as
## parameter and attempts to return its inverse from the cache. In the case, where the
## inverse matrix is not saved in the cache, it is calculated normally and saved to the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
    if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
    }
    mtx <- x$get()
    inv <- solve(mtx, ...)
    x$setinv <- inv;
    inv
}
