## These functions work together to create a simple
## method to invert a matrix. `makeCacheMatrix()`
## creates a matrix on which to operate, `cacheSolve()`
## calls to `makeCacheMatrix.getinverse() to invert the matrix.

## Create a special matrix object; provide get/set/invert functions to operate on the object.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }#set()
    get <- function() x

    setinverse <- function(x) m <<- cacheSolve

    getinverse <- function() m
    list (
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )#list
}#makeCacheMatrix()


## Compute the inverse of a matrix and cache the inverse value for easy retrieval
## if the matrix is already present in memory, the memory resident data will be
## returned without recomputation, thereby improving performance
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached matrix")
        return(m)
    }#if
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}#cacheSolve()
