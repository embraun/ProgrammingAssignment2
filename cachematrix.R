#making cached inverted matrix:

#input matrix instead of vector
makeCacheMatrix <- function(x = matrix()) { 
    i <- NULL 
    
    #set matrix value
    set <- function(y) { 
        x <<- y
        i <<- NULL
    }
    #get matrix value
    get <- function() x
    
    #set the value of the invertible matrix
    setinverse <- function(inverse) i <<- inverse
    
    #get the value of the invertible matrix
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#returning a matrix that is the inverse of x

cacheSolve <- function(x, ...) {
    
    #get value of inverted matrix from makeCacheMatrix function
    i <- x$getinverse()
    
    #if inverse matrix is not NUll do that
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    #if the value of the inversed matrix is NULL then do that
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    return(i)
}
