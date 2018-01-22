## Put comments here that give an overall description of what your
## functions do
## This program is mainly used to find the inverse of a matrix. If the inverse of a matrix is already found, it will be cached and the cached data is retrieved. If the data is not cached, then the data is cached for further retrievals.

## Write a short comment describing this function
##in this function we cache the inverse of a matrix and return a list of functions which can be used to retrieve the data
makeCacheMatrix <- function(x = matrix()) {
     inv_matrix <- NULL
     set <-function(y) {
	     x <<- y
	     inv_matrix <<- NULL
     }
     get <- function() x
     setinverse <- function(invmatrix) inv_matrix <<-invmatrix
     getinverse <- function() inv_matrix
     list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve function first checks if the inverse of matrix is cached already. If its cached, then the cached value is returned and the computation is over. If there is no cached data, then the inverse of a matrix is calculated aand cached.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv_matrix <- x$getinverse()
	if(!is.null(inv_matrix)) {
		message("getting cached data")
		return(inv_matrix)
	}
	data_matrix <- x$get()
	inv_matrix <- solve(data_matrix,...)
	x$setinverse(inv_matrix)
	inv_matrix
}
