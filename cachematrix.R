## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse,
             x = x,
             m = m)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return (m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


a <- matrix(1:4,2,2) 
> b <- makeCacheMatrix(a)
> cacheSolve(b)
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(b)
getting cached data
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5


a <- matrix(1:4,2,2)
> b <- makeCacheMatrix(a)
> b$get()
[,1] [,2]
[1,]    1    3
[2,]    2    4
> cacheSolve(b) # cacheSolve has to run to store the Inversed matrix
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> b$getMatrix()
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5