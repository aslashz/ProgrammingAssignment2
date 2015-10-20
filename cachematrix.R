## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
## params :
##     x : obj matrix
## 
## makeCacheMatrix used for create special matrix class 
## which save matrix and its inverse when called by 
## cacheSolve method
makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    get <- function() x
    setinv <- function(inv) invx <<- inv
    getinv <- function() invx
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve
## params :
##     x : obj makeCacheMatrix
## 
## cacheSolve used for retrieve inverse matrix from x, 
## if solve(x) hasn't been done before, this method will 
## calculate the inverse using solve(x) and save the result
## 

cacheSolve <- function(x, ...) {
    invx <- x$getinv()
    if (!is.null(invx)) {
        message("getting cached data")
        return(invx)
    }
    data <- x$get()
    invx <- solve(data)
    x$setinv(invx)
    ## Return a matrix that is the inverse of 'x'
    invx
}
