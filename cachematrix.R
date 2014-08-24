## Put comments here that give an overall description of what your
## functions do

## These two functions work together for computing inverse matrix efficiently. 
## Specificilly, once we have calculated one matrix's inverse, if we need it again, 
## then these functions will give the result immediately, without calculating again.

## Write a short comment describing this function

## makeCacheMatrix function is to set a special list for a matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
           x <<- y
           inv <<- NULL
           }
    get <- function() x
    setinv <- function(inv_matrix) inv <<- inv_matrix
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

## cacheSolva function is to calculate one matrix's inverse. If it has been calculated before, 
## then it will give its answer immediately. Otherwise, this function will calculate inverse and 
## record it for next use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)}
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
