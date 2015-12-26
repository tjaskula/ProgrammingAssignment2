## This file contains a pair of functions that cache the inverse of a matrix.
## Usage example:
##    c=rbind(c(1, -1/4), c(-1/4, 1))
##    fc <- makeCacheMatrix(c)
##    inv <- cacheSolve(fc)
## calling the second time 'cacheSolve(fc)' will get inverse matrix from the cache.

## Creates a special "matrix" object that can cache its inverse,
## which is really a list containing a function to
##  - set the value of the matrix
##  - get the value of the matrix
##  - set the value of the inverse of the matrix
##  - get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(a = data, ...)
        x$setinverse(inv)
        inv
}