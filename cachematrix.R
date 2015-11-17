## Below are a pair of functions used to inverse  a matrix, and return the inverse.  
## If cached, return the cached value.

## This function creates a special "matrix" object (a list) that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ##start with s being empty
    s <- NULL
    ## message("s set to null")
    
    ## function to 
    set <- function(y) {
        ## message("set called")
        x <<- y
        s <<- NULL
    }
    
    ## return the original matrix
    get <- function() {
        ## message("get called")
        x
    }
    
    ##cache the solved matrix
    setsolve <- function(solve) {
        ## message("setsolve called")
        s <<- solve
    }
    
    ## return the solved matrix
    getsolve <- function() {
        ## message("getsolve called")
        s
    }
    
    ## save the cached matrix in the list
    ## message("list")
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" (really a list) returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    
    ## check to see if the inverse is already cached
    s <- x$getsolve()
    
    ## if it is cached, return it from the cache
    if(!is.null(s)) {
        message("getting cached data")
    return(s)
    }
    
    ## if not cached, inverse it and return
    
    ## grab the data
    data <- x$get()
    
    ## inverse it
    s <- solve(data)
    
    ## save it in the cache
    x$setsolve(s)
    
    ## return it
    s
}
