## Put comments here that give an overall description of what your
## functions do

## This function, makeCacheMatrix creates a special "matrix", 
##  which is really a list containing a function to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse of the matrix
##  4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solved) m <<- solved
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## This function, cacheSolve, calculates the inverse of the special  
##  "matrix" created with the above function, makeCacheMatrix.
##  However, it first checks to see if the inverse has already been 
##  calculated. If so, it gets the inverse from the cache and skips
##  the computation. Otherwise, it calculates the inverse (if it is
##  invertible) of the data and sets the value of the inverse in the
##  cache via the setinverse function. If it is not invertible, it
##  sets the value of the inverse in the cache to NULL and returns
##  an ERROR message.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    if(!is.null(m)) {
        message("\nCACHED DATA")
        return(m)
    }
    
    data <- x$get()
    
    m <- tryCatch({
        err.flag <<- FALSE
        solve(data)
    }, warning = function(w) {
        warning(e)
        err.flag <<- TRUE
    }, error = function(e) {
        warning(e)
        err.flag <<- TRUE
    }, finally = {
    })
    
    if (err.flag) { 
        message("\n\nERROR")
        m <- NULL
        x$setinverse(m)
    } else {
        message("\nNON-CACHED DATA")
        x$setinverse(m)
        m
    }
}
