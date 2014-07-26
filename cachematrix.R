##function to create a special "matrix" object that caches its inverse.
## it starts with a NULL matrix

## the inverse of the matrix is done with the solve function
## it assumes the input is a square matrix and non singular

makeCacheMatrix <- function(x = matrix()) {
	## initialize the value of the matrix inverse to NULL
            x <- NULL
    ## delcare another function set where the value will be cached in
            set <- function(y) {
                   x<<- y
                   m <<- NULL
                  }
            get <- function() x
    #calculates the inverse of non-singular matrix via the solve function
            setinverse <- function(solve) m <<- solve
    # gets the inverse  
            getinverse <- function() m
    ## passes the value of the function makeCacheMatrix
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' using the cache
                m <- x$getinverse()
        #if the inverse exists, retrieve it
                if(!is.null(m)) {
                  message("getting cached data")
                  return(m)
                }
        #if the inverse is NULL, use solve to compute
                data <- x$get()
                m <-solve(data)
                x$setinverse(m)
                m
}
