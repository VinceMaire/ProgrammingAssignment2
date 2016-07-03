## Matrix - Inverse calculation and cache


## function to create a matrix and cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
     inver <- NULL
     set <- function(y) 
     {
         x <<- y
         inver <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) inver <<- inverse
     getInverse <- function() inver
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## computes the inverse of the matrix created by makeCacheMatrix if it is not alredy cached 
## or get the cached inversed matrix from cache

cacheSolve <- function(x, ...) 
{
        ## Return a inversed matrix of arg x
        inver <- x$getInverse()
        if (!is.null(inver))
        {
                message("from cached data:")
                return(inver)
        }
        mat <- x$get()
        inver <- solve(mat, ...)
        x$setInverse(inver)
        message("from computation:")
        inver
}
