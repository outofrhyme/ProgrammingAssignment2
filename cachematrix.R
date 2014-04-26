## Use these two functions first to create a cache of the inverse of a matrix, 
## and then to retrieve the inverse at a later time.

## makeCacheMatrix: This function takes a matrix as an input. If there is no
## matrix specified, the function will create an empty matrix. The function
## uses a different environment to set and store information about the matrix 
## and its cache in a list.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function takes a matrix as an input. It checks the cached 
## list to determine whether the inverse of the matrix has already been solved
## and stored. If the inverse is present in the cache, the function returns the
## message "getting cached data" followed by the cached values. If the inverse
## is not present in the cache, the function solves and returns the inverse, and
## also saves the inverse to the cache so it can be called in the future.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
