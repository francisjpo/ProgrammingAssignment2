## Caching the inverse of a matrix

## makeCacheMatrix caches the inverse of a matrix to save computational efforts by
## setting a matrix saving its inverse.

makeCacheMatrix <- function(x = matrix()) {

    ## Initialize the inverse
    inv <- NULL

    ## Set the value of the matrix
    set <- function( thematrix ) {
            x <<- thematrix
            inv <<- NULL
    }

    ## Get the value of the matrix
    get <- function() {x}

    ## Set the inverse of the matrix
    set_inverse <- function(inverse) {inv <<- inverse}

    ## Get the inverse of the matrix
    get_inverse <- function() {inv}

    ## List of the methods
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)

}

## Assuming the matrix is invertible, this function returns that inverse.
## Checks if it has already been calculated and returns the cached version or
## the new calculation.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat <- x$getInverse()

    ## Checks if it has already been calculated
    if( !is.null(mat) ) {
            message("getting cached data....")
            return(mat)
    }

    ## Get the matrix
    data <- x$get()

    ## Calculate the inverse
    mat <- solve(data)

    ## Set the inverse
    x$setInverse(mat)

    ## Return the matrix
    mat
}
