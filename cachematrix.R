## The main purpose of the functions below is to cache the inverse of a matrix.
## cacheSolve function calulates the inverse of a square matrix. If the inverse
## of a matrix has already been solved, it returns the cached matrix with the
## help of makeCacheMatrix function.

## makeCacheMatrix function initializes the inverse matrix inv to NULL
## and returns a list of the following functions:
## set: the matrix stored in an existing object
## created by makeCacheMatrix can be modified, inverse will be set to NULL
## get: returns the matrix stored in the object
## setinverse: with the help of <<- operator assigns value in another environment
## getinverse: returns the cached inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        # The <<- operator refers to variables
        # that already exist in some "parent" environment (makeCacheMatrix)
        # -> the variable inv is calculated in cacheSolve function
        setinverse <- function(inversed) inv <<- inversed
        getinverse <- function() inv
        ## Returns the list of explained functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve function first checks whether the inverse of a matrix has already
## been calculated and cached by the makeCacheMatrix. If not, the inverse is
## calculated for the first time and stored by the setinverse in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## the inverse of the matrix can be accessed only in the environment of 
        ## makeCacheMatrix.
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        ## If no inverse is cached, it is calculated for the first time 
        ## and cached with the setinverse function from makeCacheMatrix.
        inv <- solve(data, ...)
        x$setinverse(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}
## Test cases
print(mat <- matrix(1:4,2,2)) ## creates simple 2x2 matrix
print(mat_sp <- makeCacheMatrix(mat)) ## creates a list of four functions,  
mat_sp$get()                          ## with the matrix accessible via get
cacheSolve(mat_sp) ## calculates the inverse for the 1st time and caches it
cacheSolve(mat_sp) ## for the second time, it gest faster the cached inverse
mat_sp$getinverse()%*%mat_sp$get() ## matrix multiplication results in identity
                                                                                                     


