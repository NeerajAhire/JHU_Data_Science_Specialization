## The following function pairs cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## It's really a list containing a function to set the value of the matrix, 
## get the value of the matrix, set the the inverse of the matrix and get the
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(MatrixSolve) inverse <<- MatrixSolve
        getInverse <- function() inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" object returned by the
## makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)){
                message("getting cached result")
                return(inverse)
        }
        mat <- x$get()
        inverse <- solve(mat)
        x$setInverse(inverse)
        return(inverse)     
}
