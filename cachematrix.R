## makeCacheMatrix function creates a matrix that can cache its inverse.
## cacheSolve function computes the inverse of the above matrix.
## The first time that cacheSolve is called it computes the inverse matrix
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix Function Description:
## First it sets the matrix
## then it returns the matrix with the get function
## in the third step it sets the inverse and then returns the inverse


makeCacheMatrix <- function(x = matrix()) {
        ## set the matrix
        inv = NULL
        set = function(y) {
                # <<- assigns value to an object in the environment that is different from current environment 
                x <<- y
                inv <<- NULL
        }
        ## return the matrix
        get = function() x
        ## set the inverse 
        setinv = function(inverse) inv <<- inverse
        ## get the inverse
        getinv = function() inv
        list(set=set, get=get, 
             setinv=setinv, getinv=getinv)
}


## cacheSolve function description:
## get the output of makeCacheMatrix()
## return the inverse of the matrix
## In case the inverse has already been calculated then return the inverse from cache.


cacheSolve <- function(x, ...) {
        ##  get the output of makeCacheMatrix() function
        ## return the inverse of the matrix
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from cache and do not calculate again
                message("getting cached inverse of the matrix")
                return(inv)
        }
        
        # if inverse is not in the cache then calculate the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinv(inv)
        return(inv)
}
