## These two functions provide a caching facility for matrix inversion.
## The expected way of usage is to enhance a given invertible matrix `m`
## by calling `makeCacheMatrix(m)` and afterwards calling `cacheSolve()` for
## the enhanced matrix object.
## The internal matrix of the special cache matrix can be changed with the
## `set()` function without the need for creating a new caching matrix.
## Please note that the input matrices are expected to be invertible; no further
## data validation will take place.

## Stores and provides a matrix and the inverse of that matrix. This function
## is meant to be used in conjunction with `cacheSolve()` from below.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL

        get <- function() {
                x
        }

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        getInverse <- function() {
                inv
        }

        setInverse <- function(inverse) {
                inv <<- inverse
        }

        list(get = get,
             set = set,
             getInverse = getInverse,
             setInverse = setInverse)
}


## Returns the inverse matrix of `x` given that `x` has been created with
## `makeCacheMatrix` for an invertible input matrix. This function behaves
## exactly as the `solve()` function but in addition provides a caching
## mechanism which will return the result from cache as long as the input matrix
## does not change
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("get inverse from cache")
                return(inv)
        }
        invertibleMatrix <- x$get()
        inv <- solve(invertibleMatrix)
        x$setInverse(inv)
        inv
}
