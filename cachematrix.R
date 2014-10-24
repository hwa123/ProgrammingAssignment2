## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the Inverse Property
        i <- NULL
        ## Set Matrix
        set <- function(matrix) {
                m <<- matrix
                i <<- NULL
        }
        ## Get the Matrix
        get <- function() {
                ##Return Matrix
                m
        }
        ## Set Inverse of the Matrix
        setInverse <- function(inverse) {
                i <<- inverse
        }
        ## Get Inverse of the Matrix
        getInverse <- function() {
                ##Return the Inverse Property
                i
        }
        
        ## Return a list of the methods
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix" above. If the inverse has already
## been calculated (and the matrix has not changed), then the "cachesolve" should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## Return the inverse if its already set
        if( !is.null(m)) {
                message("getting cache data")
                return(m)
        }
        
        ## Get the matrix from our object
        data <- x$get()
        ## Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        ## Set the inverse to the object
        x$setInverse(m)
        ## Return the matrix
        m
}
