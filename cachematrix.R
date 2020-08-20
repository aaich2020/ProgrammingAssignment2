## function: makeCacheMatrix and
## function: cacheSolve -- that cache the inverse of a matrix

## Creating a matrix object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {

## Initializtion of the inverse matrix
    i <- NULL

    ## Method to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Method to get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
## Method to get the inverse of the matrix
    getInverse <- function() {
## Return the inverse
        i
    }

## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)


}


## Calculate the inverse of the matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated (and the matrix has not
## changed), then the "cacheSolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return the matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from that object
    data <- x$get()

    ## Compute the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to that object
    x$setInverse(m)

    ## Return the matrix
    m	
}
