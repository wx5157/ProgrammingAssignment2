## makeCacheMatrix creates a list of functions (set, get, setinverse, getinverse) on a matrix
## cacheSolve looks for the inverse if it's been set, otherwise, it calculates the inverse using the solve function


## Creates 4 functions (set, get, setinverse, getinverse) on a matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse=NULL
    set = function(y) {
        x<<-y
        inverse<<-NULL
    }
    get=function() x
    setinverse=function(setinversevalue) inverse<<-setinversevalue
    getinverse=function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## function testing - testing the capability of this function

mmatrix=makeCacheMatrix() ##calls the function
x=rbind(c(1, -1/4), c(-1/4, 1)) ##creates the matrix
mmatrix$set(x) ##set the matrix within the function
mmatrix$get() ##attempt to return the matrix - check vs actual matrix
mmatrix$getinverse() ##attempt to turn the inverse without it existing - should return null
mmatrix$setinverse(solve(x)) ##set the inverse to be the actual inverse
mmatrix$getinverse() ##now this function should return the actual inverse

## Create a function that return the inverse of a matrix, but checkes the cache first to see if it already exist

cacheSolve <- function(x, ...) {
    inverse=x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    matrix=x$get()
    inverse=solve(matrix, ...)
    x$setinverse(inverse)
    return(inverse)
}

##function testing - testing the capability of this function

cacheSolve(mmatrix) ##should return the actual inverse from cache since we solved it before
mmatrix$setinverse(1:4) ##sets the inverse to a non-sense value
cacheSolve(mmatrix) ##should now return the non-sense value from cache
mmatrix$set(x) ##reset x
cacheSolve(mmatrix) ##Should return the inverse but through actually solving for it

