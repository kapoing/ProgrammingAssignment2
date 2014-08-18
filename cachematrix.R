
## makeCacheMatrix
## Input: A matrix for which one will want to calculate de Inverse of.
## It is assumed that the matrix is square (NxN)
## Output: A list of 4 functions:
## * set = Set the Matrix to compute the Inverse. Receives the Matrix as Input
## * get = Get the Matrix to compute the Inverse. Null input, returns a Matrix 
## object
## * setInverse = Set the Matrix Inverse. Receives the Inverse Matrix as Input
## * getInverse = Get the Matrix Inverse. Null input
##
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## makeCacheMatrix
## Input: x should be a makeCacheMatrix function returned object
## (list of 4 functions)
## The other inputs will be forwarded to the "solve" function
## Output: The inverse Matrix
## This functions tries to use a previous calculated inverse matrix. When none 
## is available, it will calculate the inverse, store it for future use and 
## return it


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}