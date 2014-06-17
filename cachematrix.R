## Put comments here that give an overall description of what your
## functions do

## This function takes a given matrix and provides two pairs of functions.
## The first pair is used to set and get the stored matrix.
## The second pair is used to set and get the inverse of the stored matrix.
## Any time the stored matrix is set, the inverse is reset to null until
## it is explicitly set (i.e. calculated) via the setinv() function
makeCacheMatrix <- function(x = matrix()) {

	  #Initialize inv (Note matrix, x, is initialized in function call)	
        inv <- NULL

	  #set: used to set (i.e. change) the stored matrix. Also resets inv
        # Not it uses <<-, as it operates on x, inv in the function that
        # defines this function
        set <- function(y) {
                x   <<- y
                inv <<- NULL
        }

        #get: used to get the stored matrix
        get <- function() x

        #setinv: used to set the stored matrix's inverse
        setinv <- function(inverse) inv <<- inverse

        #getinv: used to get the stored matrix's inverse
        getinv <- function() inv

        #returns the list (along with x and inv)
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function takes list returned by makeCacheMatrix and 
## returns the inverse of the stored matrix, calculating it, if necessary
cacheSolve <- function(x, ...) {

        ## Attempt to retrieve inverse if already calculated
        inv <- x$getinv()

        ## If not null, return cached inverse
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        ## Otherwise, calculate the inverse
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
}
