## I give two functions "makeCacheMatrix" and "cacheSolve" to 
## cache the inverse of the matrix.

## "makeCacheMatrix" function creates a special "matrix" object 
##  that can cache its inverse.
## It create a special "vector", which is really a list containing a function to
## 1.	set the value of the vector
## 2.	get the value of the vector
## 3.	set the value of the inverse of the matrix
## 4.	get the value of the inverse of the matrix


makeCacheMatrix <- function(x=matrix() ) {

        ###initialize the inverse to NULL during the first call to makeVector
        m <- NULL
        
        ## function to set a new value for the underlying matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # getter function for underlying matrix
        get <- function() x
        
        # set the inverse of the matrix x. Called by cacheSolve
        setinverse <- function(solve) m <<- solve
        
        #returns the inverse. Will be null if setSolve has not been called or
        #if set is called after the last call to setSolve
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes a caching Vector created with makeCacheMatrix
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        # get the inverse of the matrix defined inside x.
        m <- x$getinverse()
        
        # if we've already computed the inverse and stored it via setinverse(),
        # and have not invalidated the cache by calling set(), return the cached 
        # version of the inverse

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # either we haven't computed the cached version yet, or we've called
        # set() previously and invalidated the cache.
        # call get() to get the underlying vector
        data <- x$get()
        #calculate the inverse of the underlying matrix
        m <- solve(data, ...)
        
        # now set the inverse in x so we cache it and dont need to needlessly
        #recompute it
        x$setinverse(m)
        
        # return the cached inverse
        m
}
