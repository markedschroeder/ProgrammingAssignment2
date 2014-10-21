## These functions are for making a "CacheMatrix", which is a matrix that also
## stores a cache of its inverse.

## This first function sets up the CacheMatrix, which consists of two data elements
## the matrix x and its inverse i.

makeCacheMatrix <- function(x = matrix()) {
    ## set inverse, i, to NULL when generating the Matrix object.
    i <- NULL 
    s <- function(y) {
        x <<- y
        ## set inverse, i, to NULL when setting new values to matrix, x.
        i <<- NULL 
    }
    g <- function() x
    si <- function(inverse) i <<- inverse
    gi <- function() i
    
    ## short names are internal, long names are external
    list(set = s, get = g,
         setInverse = si,
         getInverse = gi)
}


## cacheSolve is a function that either returns the precomputed inverse stored in
## the CacheMatrix, or computes it, stores it in the CacheMatrix and returns the
## value

cacheSolve <- function(x, ...) {
    ## get currently stored inverse
    i <- x$getInverse()
    
    ## check if inverse is already computed, if so return it
    if(!is.null(i)) {
        message("returning cached data")
        return(i)
    }
    
    ## if inverse has not been computed get matrix and pass it to solve
    data <- x$get()
    
    message("solving inverse of data")
    i <- solve(data, ...)
    
    ## set inverse in x to i
    x$setInverse(i)
    
    ## return i
    i
}
