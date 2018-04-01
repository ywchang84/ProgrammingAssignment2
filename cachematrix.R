## Functions for creating and using inverted matrices with caching 
## capability to reduce inverse calculation


## makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(input.matrix = matrix()) {
    result.matrix <- NULL
    set <- function(y){
        input.matrix <<- y
        result.matrix <<- NULL
    }
    ## Getting and setting matrix
    get <- function() input.matrix
    set.inverse <- function(solve) result.matrix <<- solve
    get.inverse <- function() result.matrix
    list(set = set, get = get,
         setinverse = set.inverse,
         getinverse = get.inverse)
}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(input.matrix, ...) {
    ## Return a matrix that is the inverse of 'input.matrix'
    
    ## Tries to return a cached result
    result.matrix <- input.matrix$getinverse()
    if(!is.null(result.matrix)){
        message("getting cached data")
        return(result.matrix)
    }
    
    ## No cached results, then calculate the inverse of input.matrix
    data <- input.matrix$get()
    result.matrix <- solve(data)
    input.matrix$set.inverse(result.matrix)
    result.matrix
}
