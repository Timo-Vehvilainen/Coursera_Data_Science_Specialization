## These functions allow the creation of a matrix with a list of 
## access and manipulation functions, which allow us to cache the inverse of 
## the matrix and the matrix itself

## This function creates the special vector of access and manipulation functions
## for a matrix. (get, set, get_inv, set_inv)

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        set <- function(y) {
                x <<- y
                x_inv <<-NULL
        }
        get <- function() x
        set_inv <- function(i) x_inv <<- i
        get_inv <- function() x_inv
        list(set = set, get = get,
             set_inv = set_inv, get_inv = get_inv)
}


## The following function returns a cached version of the inverse matrix of x.
## If none exists, it calculates the inverse, caches it and then returns the result

cacheSolve <- function(x, ...) {
        x_inv <- x$get_inv()
        if (!is.null(x_inv)) {
                message("getting cached result")
                return(x_inv)
        }
        data <- x$get()
        x_inv <- solve(data)
        x$set_inv(x_inv)
        x_inv
}