## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix consists of se, get, setinv, getinv
## library(MASS) is used to compute inverse for non squared as well as sqaured matrices

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setinv <- function(inverse)inv <<- inverse
        getinv <- function() {
                inver <- ginv(x)
                inver%*%x
        }
        list(set=set, get=get, setinv = setinv, getinv=getinv)
}


## Second function to get the cache data

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv ## Return a matrix that is the inverse of 'x'
}
       
