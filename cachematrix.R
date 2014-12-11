## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

# set.seed(77)
# bigVec <- makeVector(1:1000)
# biggerVec <- makeVector(1:100000)
# system.time(cachemean(bigVec))
# system.time(cachemean(bigVec))
# system.time(cachemean(biggerVec))
# system.time(cachemean(biggerVec))
# cachemean(biggerVec)
# cachemean(bigVec)
# bigVec <- makeVector(1:100)
# cachemean(bigVec)
# cachemean(bigVec)
# 
# set.seed(77)
# m <- matrix(sample.int(100,size=9,replace=TRUE), nrow=3)
# m
# d <- makeCacheMatrix(m)
# cacheSolve(d)
# solve(m)
# cacheSolve(d)
# cacheSolve(d)
# set.seed(77)
# m <- matrix(sample.int(100,size=16,replace=TRUE), nrow=4)
# d <- makeCacheMatrix(m)
# cacheSolve(d)
# cacheSolve(d)
# set.seed(77)
# m <- matrix(sample.int(100,size=10000,replace=TRUE), nrow=100)
# d <- makeCacheMatrix(m)
# system.time(cacheSolve(d))
# system.time(cacheSolve(d))
# system.time(cacheSolve(d))
# m <- matrix(sample.int(100,size=9,replace=TRUE), nrow=3)
# m
# set.seed(77)
# m <- matrix(sample.int(100,size=9,replace=TRUE), nrow=3)
# m
# d <- makeCacheMatrix(m)
# cacheSolve(d)
# d$set(m)
# cacheSolve(d)
# m <- matrix(sample.int(100,size=10000,replace=TRUE), nrow=100)
# m
# d <- makeCacheMatrix(m)
# system.time(cacheSolve(d))
# system.time(cacheSolve(d))
