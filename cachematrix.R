## Below are two functions that create a special object that stores 
## a matrix and caches its inverse.

## makeCacheMatrix creates a special "matrix" that can cache its inverse. 
## It is a list of four functions: set, get, setsolve, getsolve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## set is a function that changes the matrix stored in the main function
        
        get <- function() x
        ## get is a function that returns x stored in the main function
        
        setmatrix <- function(solve) m <<- solve
        ## setmatrix stores the value of the input in a variable m into the main 
        ## function makeCacheMatrix
        
        getmatrix <- function() m
        ## getmatrix returns the value of the variable m from the main 
        ## makeCacheMatrix function .
        
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
        ## stores the 4 functions defined above as a list
}

## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## verify the value m, stored previously with getmatrix, exists and is 
        ## not NULL. If it exists in memory, it simply returns a message and the 
        ## value m
        
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
        ## data gets the matrix stored with makeCacheMatrix, m calculates the 
        ## inverse of the matrix and x$setmatrix(m) stores it in the object 
        ## generated assigned with makeVector.

}