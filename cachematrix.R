## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function that creates a special "matrix" object that
## can cache its inverse for the input

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <-function()x
        setInverse <- function(inverse)inv <<-inverse
        getInverse <- function()inv
        list(set = set,
             get = get, 
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function
## cacheSolve is a function that computes the inverse of the returned specical
## "matrix" from the above makeCacheMarix. If the inverse is already calculated
## (and the matrix has not been altered) then cacheSolve will retrieve 
## the inverse from the cache. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached results")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
