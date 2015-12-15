## The first function makeCacheMatrix creates a list of functions applying to a matrix argument
## The second function cacheSolve returns the inverse of the original matrix argument
##
## To do this it reviews the output of makeCacheMatrix to establish whether the inverse has
## already been calculated (x$getinverse is not NULL) and if so returns the previously calculated
## value 

## this function returns a list of functions applying to the supplied argument
## it does not itself do any solving of the supplied matrix, but sets up a structure
## for cacheSolve to operate on

makeCacheMatrix <- function(x = matrix()) {
    x.inverse <- NULL
    set <- function(y) {
        x <<- y
        x.inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) x.inverse <<- solve
    getinverse <- function() x.inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve takes as argument the output of makeCacheMatrix
## Where the inverse has not previously been calculated it calls the solve()
## function on the originally supplied matrix and sets the free variable x.inverse
## to this.  On subsequent calls it finds the previously calculated value and
## returnes this without makeing any further call to solve()

## Note that in use, the output of makeCacheMatrix must be assigned to a variable
## then cacheSolve repeatedly applied to that variable.  If makeCacheMatrix(x) is
## passed directly to cacheSolve then the list is reinitialised each time it is 
## called and the cached value is reset

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    x.inverse <- x$getinverse()
    if(!is.null(x.inverse)) {
        message("getting cached data")
        return(x.inverse)
    }
    data <- x$get()
    x.inverse <- solve(data, ...)
    x$setinverse(x.inverse)
    x.inverse
}
