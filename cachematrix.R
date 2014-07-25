## This functions calculate the inverse of a matrix using the solve() and save it in a cache
## function. If the inverse had been already calculated. The function return the cached value
## Example:
## m<-makeCacheMatrix( matrix(c(1,2,3,4),nrow=2,ncol=2)) 
## cacheSolve(m)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
## If you need the inverse again
## cacheSolve(m) return the value from the cache
## and emit the message 'getting cached inverse matrix'


## Create a new type of matrix enable to save its inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
        x <<- y
        m <<- NULL
     }
     get <- function() x
     setinversematrix <- function(inversematrix) m <<- inversematrix
     getinversematrix <- function() m
     list(set = set, get = get,
          setinversematrix = setinversematrix,
          getinversematrix = getinversematrix)
}


## Return the inverse of a matrix ( using the solve function ). 
## It takeS a special type of matrix ( cache matrix ) 
## created with the  makeCacheMatrix. Check the cache before the 
## repeat the calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinversematrix()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversematrix(m)
        m
}
