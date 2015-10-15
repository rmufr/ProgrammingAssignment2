## This script defines 2 functions to 
##   - cache a matrix and its inverted matrix
##   - return the cached calculated inverted matrix if available
##      or compute it and cache it

## makeCacheMatrix returns a list of functions to cache & read a matrix
## and its inverted matrix

makeCacheMatrix <- function(x = matrix()) {

    # init the inverted matrix cached result
    inv <- NULL
    
    # define set() routine to cache input matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL # nulls the inverted matrix when input changes
    }
    
    # define get() routine to read cached input matrix
    get <- function() x
    
    # define setinv() routine to cache result of matrix inversion
    setinv <- function(i) inv <<- i
    
    # define getinv() routine to read the cached result of matrix inversion
    getinv <- function() inv
    
    # return the 'pointers' list to previously defined functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## Compute & cache, or if cached get, the inverted matrix from the cached
## values within the object returned by makeCacheMatrix function. 
## ... are passed through to solve() function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    
    solve.invm<-function(m, ...) {
        ##  wrapper to solve function for matrix inversion
        
        # verify that input is square matrix or exit with error message
        d<-dim(m)
        if (length(d)!=2 || !all(d==d[1])) {
            # m must be a NxN dimensional matrix
            stop("input is not a square matrix")
        }
        
        # make identity matrix of the same dimensions as m
        idt <- diag(d[1])
        
        # call solve() to compute the inverted matrix
        solve(m, idt, ...)
    }
    
    # check for cached result & return if found
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # otherwise get the input matrix & compute the inverted matrix
    data <- x$get()
    inv <- solve.invm(data, ...)
    
    # cache the result of above calculation
    x$setinv(inv)
    
    # return this result
    inv
}
