# Uses the example calculation mean of the vector 
# Makes use of the <<-
# Assumes the matrix is always invertible

makeCacheMatrix <- function(x = matrix()) {
        xi  <- NULL
        set  <- function(y){
                x <<- y
                xi <<- NULL 
        }
        get  <- function() x
        setinv  <- function(inv) xi  <<- inv
        getinv  <- function() xi
        list(set= set, get = get, 
             setinv = setinv, 
             getinv = getinv)

}


# Computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        xi  <- x$getinv()
        if (!is.null(xi)){
                message("Returning from cache")
                return(xi)
        }
        data  <- x$get()
        xi  <- solve(data, ...)
        x$setinv(xi)
	message("Inverse of the matrix")
	message("=====================")
        xi
}
