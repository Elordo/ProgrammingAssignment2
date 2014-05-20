## The purpose of this file is to "cache" the contents of a potentially time consuming
## calculation as the inverse of a matrix is.
## To do so, we will create 2 functions makeCacheMatrix and cacheSolve
## 
## To execute:
## invMat <- makeCacheMatrix(aMatrix)
## cacheSolve(invMat)

## In the first function, we are creating actually a list that basically
## contains other functions

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(solve) m <<- solve
        
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## In the second function, we will check if the mean is already calculated (cached)
## If it is not, then it will calculate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}
