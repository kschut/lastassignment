## 2014-08-26
## JJS
## cachematrix.R: 
##
## R-Programming assignment for week 4
## routines for calculating the inverse of an inversible matrix
## special is that if the inverse is in cache, that will be used
##
## Input is a nxn matrix which is (reasonably) invertible
## (how reasonable is a field out of scope of this class)
## return is the nxn inverse thereof
## if pulled from cache, a message shows that

## usage:
## create an nxn matrix e.g. my_m=matrix(1:4,nrow=2,ncol=2)
## call the make-function e.g. m = makeCacheMatrix(my_m)
## call the inverse-fie e.g. cacheSolve(m)

## hard test :
## my_m=matrix(c(rep(c(2,1,rep(0,9998),1),9999),2),nrow=10000,ncol=10000)
## m=makeCacheMatrix(my_m)
## inv=cacheSolve(m)   <-- takes approx 10 min CPU time 
## inv=cacheSolve(m)   <-- takes close to 0 time

## note: I do not understand this code whatsoever

## makeCacheMatrix: function which creates a matrix object that can
##                  cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	m = NULL                               # initial dummy val
        set = function(y) {                    # set matrix
                x <<- y             
                m <<- NULL
        }
        get = function() x                     # get matrix
        setsolve = function(solve) m <<- solve # set inverse
        getsolve = function() m                # get inverse
        list(set = set, get = get,	       # return list of functions
             setsolve = setsolve,
	     getsolve = getsolve)
}


## cacheSolve: function which returns the inverse of a matrix
##             if the inverse is in cache, that is returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m = x$getsolve()      # look for a chached value
        if(!is.null(m)) {     # if one is found:
                message("getting cached data")
                return(m)     # cached value gets returned
        }                     # else .....
        data = x$get()        # matrix gets read
        m = solve(data, ...)  # inverse gets calculated
        x$setsolve(m)         # inverse gets cached for later
        m                     # inverse gets returned
}


makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {		     # set data
                x <<- y
                m <<- NULL
        }
        get <- function() x                  # get data
        setmean <- function(mean) m <<- mean # set mean
        getmean <- function() m              # get mean
        list(set = set, get = get,           # return list of functions
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()           # look for a cached mean
        if(!is.null(m)) {          # if one is found:
                message("getting cached data")
                return(m)          # return mean
        }                          # else .....
        data <- x$get()            # get data
        m <- mean(data, ...)       # calculate mean
        x$setmean(m)               # cache mean for later use
        m                          # return mean
}