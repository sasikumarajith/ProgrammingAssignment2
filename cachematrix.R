## makeCacheMatrix function creates a list of functions to set the matrix variable, get the matrix variable, and also set/get its inverse and
## cacheSolve function uses the function solve to get the inverse of the inverstible square matrix if an inverse has not already been calculated

## Function takes a matrix as an input.Return a list of functions to set /get the variable or set / get the inverse

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve function calcualtes inverse of the square invertible matrix if one is already not present.

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
