## This script takes a matrix, creates a memory cache, performs the inverse operation
## on the matrix and stores it in the cache to reduce repetitive computation time

## This function takes a matrix as an input creates a memory cache 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve<- function(solve) m <<- solve
        getsolve<- function() m
        list(set = set, get = get,
             setsolve= setsolve,
             getsolve = getsolve)
}


## This function takes a makeCacheMatrix() object (which contains an original
## matrix), performs an inverse operation on the matrix if the operation has not
## previously been run, or takes the the result from the memory cache if previously
## run, and returns the inverse matrix

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
