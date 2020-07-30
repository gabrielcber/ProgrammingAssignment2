## Sometimes when people are analyzing data, it's well advised
## to cache results to avoid time-consuming computations. Here
## in this code we are able to see how 2 functions work. 

## FUNCTIONS:
## makeCacheMatrix is responsible to set and store the matrix
## we're interested in calculating the inverse of. It also 
## stores the inverted matrix after the operation.

makeCacheMatrix <- function(x = matrix()) {

        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }       
        get <- function() x
        setinv <- function(inv) I <<- inv
        getinv <- function() I
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)     
        
}

## cacheSolve is able to operate the matrix inversion itself.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getinv()
        if(!is.null(I)){
                message("Getting cache!")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinv(I)
        I
}
