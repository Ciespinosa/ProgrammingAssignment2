## These functions create, store in the cache and recover the cache data

## This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                    mInv <- NULL
                    get <- function() x
                    
                    setInv <- function(solve) mInv <<- solve
                    getInv <- function() mInv
                         list(get = get,
                         setInv = setInv,
                         getInv = getInv)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cacheWrite 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                mInv <- x$getInv()
                 if(!is.null(mInv)) {
                message("getting cached data")
                 return(mInv)
                        }
                data <- x$get()
                mInv <- solve(data, ...)
                x$setInv(mInv)
                mInv
}
