## The Below functions demonstrate the principle of caching via lexical closure.
## This can speed up program execution by avoiding recomputing values.
## The functions below are modifications of the examples given for caching the
## mean of a vector.


## This function takes a matrix and returns a list containing:
## "set" and "get" - functions to set and get the value of the matrix.
## "setinverse" and "getinverse" - functions to comptute and retreive the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL #defines m and sets it to NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse ## sets m to the value of "inverse" in the environment where m is defined
    getinverse <- function() m #returns m (as opposed to the uninverted x)
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This as an extension of the R "solve" function to handle a list returned
## by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) { #if m is defined
        message("getting cached data")
        return(m)
    }
    mat <- x$get() #otherwise
    m <- solve(mat, ...) #invert m
    x$setinverse(m) #and cache the value
    m #now return m
}
