## makeCachematrix creates a matrix structure that returns a list of possible operations that can be 
## performed on the matrix

## creates an object with the following functions: 
## set - set matrix (also deletes cached solution)
## get - gets the matrix
## setinverse - sets the inverse
## getinverse - gets the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    minverse <- NULL
    set <- function(y){
        x <<- y
        minverse <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) minverse <<- inverse
    getinverse <- function() minverse
    list(set = set, get = get,
         setinverse = setinverse, 
         getinverse = getinverse)
    
}


## Cachesolve checks the cache to see if the calculation has already been performed. If so, it will
## retrieve the solution from the cache. If not, it calculates the solution and stores it in the cache. 
## Returns the matrix inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    minverse <- x$getinverse()
    if(!is.null(minverse)){
            message("Checking Cache")
            return(minverse)
    }
    data <- x$get()
    minverse <- solve(data, ...)
    x$setinverse(minverse)
    minverse
}
