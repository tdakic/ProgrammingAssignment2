## These functions compute and cache the inverse matrix of a square 
## invertable matrix.
##
## Usage: If you want to compute and cache the inverse of your matrix a, 
## first you need to create a new matrix with newMatrix <- makeCacheMatrix(a)
## and then you can calculte or (get the cached inverse) by cacheSolve(newMatrix)


## Creates a variable m and a list of functions for a matrix x. Variable m 
## stores the inverse matrix of the matrix x or NULL if that has not been 
## computed yet
##
## The funtions are: 
##      set: sets the value of x to the given parameter y and sets m to NULL 
##      get: returns the value of x
##      setinverse: sets the value of m to the given parameter inverse
##      getinverse: returns the value of m (inverse of x)

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL ## stores the inverse of x
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) m <<- inverse
        
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks to see if the inverse of the matrix x was cached and
## if it is, the function returns the cached value. It it is not, it computes 
## the inverse and caches it

cacheSolve <- function(x, ...) {
        
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
