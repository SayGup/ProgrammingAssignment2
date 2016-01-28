## Caching the Inverse of a Matrix
## caching the inverse of a matrix saves computation time as it avoids repeated computation

## makeCacheMatrix function creates a special “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}


## cacheSolve computes the inverse matrix created by makeCacheMatrix
## If the inverse has already been calculated and matrix unchanged, cached value returned
## Fresh computation if new/ changed matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        ## Inversing new matrix
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}
