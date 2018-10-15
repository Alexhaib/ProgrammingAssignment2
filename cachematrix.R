## Two functions working together to cache and use cached results of 
## potentially time-consuming operation of inverting (solving) an invertible square matrix.
## (FOr the purpose of this assignment we assume that the matrix provided is 
## always invertible)
## 
## The 1st function (makeCacheMatrix) is used 
## for getting the argument matrix (the one to be solved)
## and for storing the cached solved matrix. 
## It returns the LIST of named objects, all of them are 
## nested functions. Therefore to actually get the result of solved matrix we will need 
## the second function (see description below).

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) { #setter for the argument matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x #getter for the argument matrix
    setsolve <- function(solved) m <<- solved  #setter for the solved matrix
    getsolve <- function() m #getter for the solved matrix
    list(set = set, get = get, #named list storing 4 getter and setter functions 
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve takes as an argument the named list that is returned by makeCacheMatrix.
## it checks whether the cached solved matrix is empty. 
## If not empty => gets cached result
## and does not perform calulations.
## If empty - solves the matrix, and puts the result into setsolve object, then 
## returns the calculated result.

cacheSolve <- function(x, ...) {
    m <- x$getsolve() # get cached solved matrix
    if(!is.null(m)) { #if not NULL => print out the cache and break. 
        message("getting cached data")
        return(m)
    }
    data <- x$get() # else, if the cached result is absent, then compute solved matrix
    m <- solve(data, ...)
    x$setsolve(m) #put it to cache
    m # print out the result
}
