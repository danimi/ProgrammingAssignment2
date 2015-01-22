# These two function use to reduce a computation by caching the inverse of a matrix instead to repeatedly compute it.

#Function makeCacheMatrix() creates a special "matrix" object that can cache its inverse.
#This special "matrix" object is a list containing a function to 
#	1. set the value of the matrix
#   2. get the value of the matrix
#   3. set the value of the inverse matrix
#   4. get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(z) inv <<- z
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


#Function cacheSolve() returns inverse matrix of special "matrix" created with the above function. 
#If the inverse matrix has already been calculated it skips the computation and gets it from the cache
#If there is no calculated inverse matrix in the cache then cacheSolve() function calculates the inverse matrix and sets it in the cache using the setinv function.

cacheSolve <- function(x, ...) {
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