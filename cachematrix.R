## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The intention of this function is to create a special "matrix" that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    #stores the cache value and initializes 
    set <- function(y) {
      x <<- y
      cache <<- NULL
    }
    #creates the matrix in the working environment 
    get <- function() x
    #inverses the matrix and stores cache
    setMatrix <- function(inverse) cache <<- inverse
    #retrieves cached matrix invert
    getInverse <- function() cache
    
    #returns functions above to parent environment 
    list(set = set, get = get, setMatrix = setMatrix, getInverse = getInverse)

}


## Write a short comment describing this function
## computes the inverse of the special "matrix" returned by makeCacheMatrix utilizes a cachhe function
## to avoid redundant computations 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cache <- x$getInverse()
    
    ##checks cached data for a possible match 
    ##if there is a match returns inverse of 'x'
    ##if there is not a match, a new computation is executed 
    if(!is.null(cache)) {
      message("fetching from cached data")
      #prints cached matrix to console
      return(cache)
    }
    ##matrix must be square and invertible this is assumed to be true
    matrix <- x$get()
    #computes the inverse of the squared matrix
    cache <- solve(matrix, ...)
    
    x$setMatrix(cache)
    return(cache)
}
