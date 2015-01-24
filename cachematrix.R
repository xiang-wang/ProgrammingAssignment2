## Below are two functions that are used to create a special object 
## that stores a matrix and cache's its inverse matrix.

## The first function, makeCacheMatrix creates a special "matrix", which is 
## really a list containing a function to:
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse matrix
##      4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. However, it first checks to see if the inverse matrix 
## has already been calculated. If so, it gets the inverse matrix from the cache
## and skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv        
}
