## These functions implement a "cached" matrix, which stores
## also the inverse of the matrix (if calculated)

## TV 2016-01-23

## Return a "cache matrix" from matrix `x`
makeCacheMatrix <- function(x = matrix()) {
    
    minv <- NULL
    
    # Set the value of the matrix to `mat`, reset the inverse cache
    set <- function(mat) {
        x <<- mat
        minv <<- NULL
    }
    
    # Return the stored matrix
    get <- function() x
    
    # Set the inverse of the matrix to `inv`
    setinverse <- function(inv) minv <<- inv
    
    # Return the inverse of the matrix (or NULL, if not cached)
    getinverse <- function() minv
    
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


## Return the inverse of the cached matrix `x`, either from cache
## or fresh from solve()
## Extra arguments are passed directly to `solve` function in base

cacheSolve <- function(x, ...) {

    # Get the stored inverse, return it if known
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Calculate the inverse matrix (was not cached) and store it
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    
    # Return the inverse
    inv
}
