## Name: cachematrix.R
## Description: two functions that solve matrix inversions using a cache
## Author: Justin Myers
## Date: 15 June 2015
## Status: Production

## This function creates and handles a cache for use by cacheSolve function
makeCacheMatrix <- function(storedInput = matrix()) {
        cache = NULL
        #set function stores the new matrix and zeroes out the cache
        set <- function(inputMatrix){
                storedInput <<- inputMatrix
                cache <<- NULL
        }
        
        #get function returns the current matrix
        get <- function() storedInput
        
        #setInv function saves the computed value to the cache
        setInv <- function(inv) cache <<- inv
        
        #getInv returns the cached value (may be NULL)
        getInv <- function() cache
        
        #initialize all functions
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function returns a matrix that is the inverse of 'x', using a cache 
## lookup if possible
cacheSolve <- function(x, ...) {
        #try a cache lookup.  if it works, return the cached data
        cacheData <- x$getInv()
        if (!is.null(cacheData)){
                message("Used cached data.")
                return(cacheData)
        }
        
        #otherwise, compute the inverse, save it to the cache, and return it
        data <- x$get()
        inv <- solve(data)
        x$setInv(inv)
        inv
}
