## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix makes a special matrix
## which is a list containing a function to 
## 1. set the inverse of the matrix
## 2. get the inverse of the matrix
## 3. set the inverse
## 4. get the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(invMatrix) m <<- invMatrix
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The function takes as a parmeter the cache matrix
## and tests to see if the inverse of the matrix has been 
## calculated.  If the inverse of x has been cached
## then that value is returned, otherwise the inverse
## of x is calculated, cached and then returned.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
