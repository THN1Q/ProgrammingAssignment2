## A set of functions which calculate and cache the inverse of a matrix for
## later usage.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL 
        
        ## this object stores the inverse of the matrix
        
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        ## stores the matrix in the global environment and resets inverse matrix
        
        get <- function () x
        
        ## prints the matrix
        
        setinverse <- function(inverse) i <<- inverse
        
        ## stores inverse matrix of x in the global environment
        
        getinverse <- function() i
        
        ## prints inverse matrix of x
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

        ## function interface for makeCacheMatrix
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated, it retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## checks for cached inverse matrix of x and returns it if available       
        
        data <- x$get()       ## retrieves original matrix
        i <- solve(data, ...) ## calculates inverse matrix of x
        x$setinverse(i)       ## stores calculated inverse matrix in GE
        i                     ## printes inverse matrix
}