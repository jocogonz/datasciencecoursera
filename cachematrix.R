## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { 
        
        ## This function creates a special "matrix" object that can cache its inverse
        ## Set the default value of x to be a "matrix"
        
        ## Need a place holder for the inverse value "inverseM" so we initialize
        ## inverseM to NULL
        
        inverseM <- NULL                             
        
        ## define the set function to assign new value of matrix in parent environment
        set <- function(y) {                
                x <<- y                        
                inverseM <<- NULL
        }
        
        get <- function() x                   
        setinverseM <- function(inverseM) inverseM <<- inverse
        getinverseM <- function() inverseM                  
        list(set = set, get = get, 
             setinverseM = setinverseM, getinverseM = getinverseM)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseM <- x$getinverseM()
        if(!is.null(inverseM)) {
                message("getting cached data")
                return(inverseM)
        }
        data <- x$get()
        inverseM <- solve(data, ...)
        x$setinverseM(inverseM)
        inverseM
}
