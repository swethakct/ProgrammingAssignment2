## Put comments here that give an overall description of what your 
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                  # Initialise inv as NULL
        set <- function(y) {                         # Define the set function and assign new function to y
                x <<- y                              # Value of matrix in Parent environment
                inv <<- NULL                         # If there is a new matrix, reset inv
        }
        get <- function() x                          # Define the function
        setinverse <- function(inverse) inv <<- inverse # assigns the value of inv
        getinverse <- function() inv                  # gets the value of inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
}


## Write a short comment describing this function
# then cacheSolve will retrive the inverse from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(inv)
        inv
}
}
