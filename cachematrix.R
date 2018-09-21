## The makeCacheMatrix function creates a matrix object that can cache its inverse.
## The cacheSolve funtion computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the inverse is retrieved from the cache.

## makeCacheMatrix creates a matrix(x), calculates the inverse and sets the 
## inverse value to the cache.

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve returns the inverse of the original matrix input to makeCacheMatrix()
## if the inverse has already been calculated (the matrix is unchanged) the value 
## is retrieved from the cache and not calculated. If not, the value is calculated.

cacheSolve <- function(x, ...) {
        inv = x$getinv()
        
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        x$setinv(inv)
        
        return(inv)
}
