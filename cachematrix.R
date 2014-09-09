## Rachel Adams
## Coursera: R Programming
## prog-007
## Assignment 2: Lexical Scoping
## cachematrix.R

## Computing the inverse of a matrix could be time-consuming
## These two functions create a special "matrix" and return its inverse.
## The inverse is computed only once and then cached; each subsequent call
##   to invert/solve the matrix simply displays the cached inverse matrix.

## makeCacheMatrix creates a special "matrix", which is really a list that
##   contains functions/elements to do the following:
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse
##   - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    ## 'x' is an invertible matrix 
    
    ## 'm' is the inverse of the matrix
    ## initially 'm' is NULL
    m <- NULL
    
    ## the 'set' function sets/modifies the contents of the 'x' matrix
    ## if 'x' is modified, the inverse matrix 'm' is set to NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## the 'get' function displays the contents of the 'x' matrix
    get <- function() x
    
    ## the 'setinverse' function sets/modifies the 'm' matrix
    ##   the 'm' matrix is the inverse of the 'x' matrix
    ##   the 'solve' function returns the inverse of a matrix
    setinverse <- function(solve) m <<- solve
    
    ## the 'getinverse' function displays the 'm' matrix
    getinverse <- function() m
    
    ## name the elements in the special "matrix", which is really a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    
    ## 'x' is a "special" matrix created by the makeCacheMatrix function
    
    ## 'm' is the inverse matrix of 'x'
    ## if 'm' has not been computed before, it will be NULL
    ## -- note: 'm' will also be NULL if 'x' has changed values since 'm' was last computed
    m <- x$getinverse()
    
    ## if 'm' has been computed for this 'x' before, it will not be NULL
    ## 'm' does not need to be computed again
    ## a message will be printed to indicate that 'm' has already been computed
    ## the function will return the matrix 'm' at the end of the if statement
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## the inverse matrix 'm' has not been computed before
    ## get the values of the original matrix 'x'
    data <- x$get()
    
    ## compute the inversion of the 'x' matrix and save to 'm'
    m <- solve(data, ...)
    
    ## set the inverse of 'x' to 'm' 
    ## after this, the getinverse function will not return NULL
    ## and therefore, 'm' will not need to be computed again
    x$setinverse(m)
    
    ## return the inverse matrix 'm'
    m
}
