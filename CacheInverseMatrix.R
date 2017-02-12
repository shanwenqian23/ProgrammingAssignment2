## The inefficiency of repeating the same computations is obvious 
## and is always a problem to solve for programmers. 
## These two functions below are used to store the matrix object  
## and cache the result of the inverse matrix operation for future looking up.

## The first function makeCacheMatrix() creates a list 
## containing function set(), get(), setsolve() and getsolve() 
## to set new object matrix and result, get the object and result.
## Most importantly, it helps to store the matrix in the cache.

makeCacheMatrix <- function( x = matrix()){
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function()  x
        setsolve <- function(solve)  s <<- solve 
        getsolve <- function()  s
        list(set = set, get = get, 
             setsolve = setsolve, 
             getsolve = getsolve)
}

## The second function cacheSolve() is able to get the inverse of the matrix
## which we assigned in the first function.
## This function will check if the inverse matrix has existed in the cache before any computation.
## If so, it will return the result in the cache. If not, it will give the result from new computation. 

cacheSolve <- function(x, ...){
        s <- x$getsolve()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
                ## Return a matrix that is the inverse of 'x'
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
