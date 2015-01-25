## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        Inverse <- NULL
        set <- function(y) {
                x <<- y
                Inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inv) Inverse <<- inv
        getinv <- function() Inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then `cacheSolve` should retrieve the inverse from the cache.
## If the inverse of the matrix was not calculated before, then the function calculate the inverse
## and cache the result.

cacheSolve <- function(x, ...) {
        Inverse <- x$getinv()
        if(!is.null(Inverse)) {
                message("Getting cached data")
                return(Inverse)
        }
        data <- x$get()
        Inverse <- solve(data, ...)
        x$setinv(Inverse)
        Inverse
}
