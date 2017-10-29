## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than
## compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
        # Part 1: Set the value of the vector
        set <- function (mtx) {
                x <<- mtx
                invMatrix <<- NULL
        }
        # Part 2: Get the value of the vector
        get <- function() x
        
        # Part 3: Set the value of the inverse
        setInverse <- function(newInverse) invMatrix <<- newInverse
        
        # Part 4: Get the value of the inverse
        getInverse <- function() invMatrix 
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)   
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInverse()
        if (!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        mat <- x$get()
        invMatrix <- solve(mat, ...)
        x$setInverse(invMatrix)
        invMatrix
}
