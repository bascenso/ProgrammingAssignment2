## R Programming course - Programming Assignment 2: Lexical Scoping
## 
## This assgnment includes two functions:
##  -- makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
##  -- cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##                 If the inverse has already been calculated (and the matrix has not changed), then it is retrieved from the cache


## This function creates a special "matrix" object that can cache its inverse.
## The special matrix object contains the functions:
##    -- get: to retrieve its data
##    -- set: to set the data; the inverse is not computed immeditelly as it may be a lenghty operation
##    -- setInverse: to compute the inverse and cache it.
##    -- getInverse: to retrieve the inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    get <- function() x ## Returns the matrix data
    
    set <- function(m) { ## Sets the matrix data and clears the inverse
        x <<- m
        inverse <<- NULL
    }
    
    setInverse <- function() { ## Computes the inverse and 'caches' it
        inverse <<- solve(x)
        
    }
    
    getInverse <- function() inverse ## Returns the inverse
    
    list (set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## This function computes the inverse of the special "matrix" created by makeCacheMatrix above
## Receives a special matrix as argument, previously created with the function above
## Returns a matrix with the inverse
cacheSolve <- function(x, ...) {

    inv <- x$getInverse()
    
    if (!(is.null(inv))) return (inv)  ## if the inverse exists in cache return it
    
    ## else
    message("Inverse not previously cached. This may take a while...\n")
    x$setInverse() ## if not call the function to compute the inverse
    
    x$getInverse() ## and return it
}

