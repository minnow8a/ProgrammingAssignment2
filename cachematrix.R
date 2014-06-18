## Put comments here that give an overall description of what your
## functions do

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(input_matrix = matrix()) {
        inverse_matrix <- NULL
        set <- function(x) {
                input_matrix <<- x
                inverse_matrix <<- NULL
        }
        get <- function() input_matrix
        setinverse <- function(y) inverse_matrix <<- y
        getinverse <- function() inverse_matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## computes the inverse of the matrix returned by makeCacheMatrix. If the inverse has already been
## calculated (and the matrix has not change), then the cachesolve would retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mem_inverse <- x$getinverse()
        if(!is.null(mem_inverse)) {
        	message("getting cached data")
        	return(mem_inverse)
        }
        data <- x$get()
        mem_inverse <- solve(data)
        x$setinverse(mem_inverse)
        mem_inverse
}
