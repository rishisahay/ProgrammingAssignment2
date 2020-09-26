
## The two functions below, makeCacheMatrix() and cacheSolve()
## together enable the user to set a matrix and its inverse in
## the cache.  See the description of the two functions below
## for more detail.

## makeCacheMatrix() creates an R object that stores a matrix
## and its inverse (or more accurately a placeholder 
## for its inverse) and returns a list of four functions which
## get and set the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setinverse <- function(matrixinverse) {inv <<- matrixinverse}
    getinverse <- function() {inv}
    list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve() takes an object that is outputted from
## makeCacheMatrix() as input, checks if a placeholder inverse
## is cached and returns it if so.  If not it gets the matrix
## stored in the output of the R object created by makeCacheMatrix(),
## calculates the inverse of this matrix,
## stores that inverse as the new placeholder inverse,
## and returns that inverse.

cacheSolve <- function(x, ...) {
    ##if inv is not null, return it, 
    ##otherwise calc inverse, cache it, and return it
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
