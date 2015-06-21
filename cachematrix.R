## Since matrix inversion can be time-consuming, you may want to cache the inverse of a matrix rather than computing it repeatedly. 
## This is a pair of functions that cache the inverse of a matrix.

## The makeCacheMatrix function creates a special object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL          
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve) 
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
                
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
