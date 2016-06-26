## These two functions calculate the inverse of a matrix and cache 
## it into memory



## This function creates a cache in memory for the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    
    #checks to see if variable "m" is initialized
    if(!exists("m")){
        m <- NULL
    }
    
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## Calculates inverse of matrix 'x' if x inverse not already stored
## in memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m <<- m
    m
}


## Example from console:

## clears value of 'm' from memory:
## > m <<- NULL

## initializes matrix 'dave':
## > dave
##      [,1] [,2] [,3]
## [1,]    1    5    4
## [2,]    2    5    2
## [3,]    3    8    8

## runs functions to solve matrix & store solution to memory:
## > cacheSolve(makeCacheMatrix(dave))
## [,1]       [,2]       [,3]
## [1,] -1.09090909  0.3636364  0.4545455
## [2,]  0.45454545  0.1818182 -0.2727273
## [3,] -0.04545455 -0.3181818  0.2272727

## re-runs function (note 'getting cached data' below) - 
## matrix solution loaded from memory:
## > cacheSolve(makeCacheMatrix(dave))
## getting cached data
## [,1]       [,2]       [,3]
## [1,] -1.09090909  0.3636364  0.4545455
## [2,]  0.45454545  0.1818182 -0.2727273
## [3,] -0.04545455 -0.3181818  0.2272727
