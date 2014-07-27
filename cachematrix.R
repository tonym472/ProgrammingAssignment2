

## This function creates a special "vector" that sets the inverse of matrix 
## and gets the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        ## initialize the value of the matrix inverse to NULL
        m <- NULL
        ## delcare another function set where the value will be cached. Matrix is created
        ## for the first time. 
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        ## gets the value of the inverse
        get <- function() x
        #calculates the inverse of the matrix via the solve function
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        #creates a list of values
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


# This function is used to get the cache of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##if the inverse exists, it returns it.
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return (m)
        }
        #if the inverse does not exist, it is calculated and then retrieved.
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


