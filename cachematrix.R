# makeCacheMatrix creates a list that sets a matrix, gets it, sets its inverse and gets its value 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse 
        getinverse <- function() i
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## cacheSolve will either give us the inverse of the matrix if it's already been calculated, or will calculate it using the solve function

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
