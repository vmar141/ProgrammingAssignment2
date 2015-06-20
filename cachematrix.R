## Functions to create a matrix that caches its inverse.
## Creates a special "matrix" object that caches the inverse of
## a matrix.

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL  ##holds matrix inverse
        
        ## sets matrix and nulls inverse
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x    ## returns matrix
        
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##      The following function calculates the inverse of the special "vector" created with the makeCacheMatrix function.
##      1) checks to see if the inverse has already been calculated. 
##      2) Then, it either gets the inverse from the cache and skips the computation. 
##      3) Or, it calculates the inverse 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- inverse(data, ...)
        x$setinverse(m)
        m
}
