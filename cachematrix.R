## cachematrix.R 
## R Programming -- Programming Assignment 2
## cachematrix.R contains 3 functions
##
## makeCacheMatrix accepts an optional normal matrix and returns "special" matrix
##                 with cached variables. Function implements assessors to check 
##                 and manipulate property values (get/set)
##
## cacheSolve accepts a special matrix as created by makeCacheMatrix. cacheSolve 
##            returns the inverse of the special matrix when that is known,
##            otherwise computes, sets and returns the inverse of the matrix
##
## runTest generates a matrix and calls makeCacheMatrix and cacheSolve. Optional
##         logical argument will force cacheSolve to throw an error on solve()


## accepts a matrix and sets initial values. 
## exposes property functions to access object values
## set() explicitly resets inverse. Don't need to test for change

makeCacheMatrix <- function(x = matrix()) {
    
    # properties
    set <- function(y) {
        xi <<- y  # set proprety value of matrix
        ix <<- NULL # default null value, resets when set() is called
    }
    get <- function() xi # get matrix
    
    setInverse <- function(y) ix <<- y  # set inverse, not checked for validity
    getInverse <- function() ix    
    
    isSpecialMatrix <- function () TRUE # just a quick flag
    
    # set as default, constructor without explicit this.set(x) call
    set(x)
    
    # list available functions
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse,
         isSpecialMatrix = isSpecialMatrix)
}


## accepts a "special" matrix
## determines if inverted cache value exists by testing for NULL, 
## sets inverted value when cache value is NULL

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
   
    # throws error when this propety function doesn't exist
    if (x$isSpecialMatrix()) {
    
        if(!is.null(x$getInverse())) {
            # if the getInverse is not NULL, the matrix has not been changed
            # a call to set() resets the inverted cache variable
            message("getting cached data")
            return(x$getInverse())
        } else {
            # solving and setting the inverse
            # will throw error when the matrix can be solved
            x$setInverse(solve(x$get(),...))
            message("caching data")
            return(x$getInverse())  
        }
        
    } else {
        # won't get here, isSpecialMatrix() will throw
        # error when referenced by regular matrix
        message("argument is not a special matrix")
    }    
}


## just a simple tes to generate some response
runTest <- function(makeError = FALSE) {
    if (makeError) {
        mdat <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3, byrow = TRUE,
                  dimnames = list(c("row1", "row2"),
                  c("C.1", "C.2","c.3")))
    } else {
        mdat <- matrix(c(1,2, 11,12), nrow = 2, ncol = 2, byrow = TRUE,
                  dimnames = list(c("row1", "row2"),
                  c("C.1", "C.2")))
    }
    
    # simple test
    cm <- makeCacheMatrix(mdat)
    print(cat("GET : ", cm$get()))
    print(cat("GET INvERSE : ", cm$getInverse()))
    cacheSolve(cm)
    print(cat("GET : ", cm$get()))
    print(cat("GET INvERSE : ", cm$getInverse()))
    cacheSolve(cm)
    
}