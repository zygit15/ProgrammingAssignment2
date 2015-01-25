## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function generates a special matrix that is a list of 
## four functions, set, get, setinv and getinv

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(z) inv <<- z
        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function
## This function calcuates the inverse of the matrix that is
## generated above. If matrix changes, the above "makeCacheMatrix"
## has to be called again to generate new 'cachematrix' to be solved
## by "cacheSolve"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cache data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
