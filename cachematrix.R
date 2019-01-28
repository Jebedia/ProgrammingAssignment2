## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## function with default argument of "matrix"
    inv <- NULL                             ## inv as NULL initialized - value of matrix inverse will bestored here 
    set <- function(y) {                    ## define set function to assign new 
        x <<- y                             ## value of matrix in parent environment
        inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
    }
    get <- function() x                     ## get function - returns value of the matrix argument
    
    setinverse <- function(inverse) inv <<- inverse  ## assigns value of invertible matrix in parent environment
    getinverse <- function() inv                     ## gets the value of invertible matrix where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## needed for referencing functions with the $ operator
}

## Write a short comment describing this function

## cacheSolve function will compute inverse of matrix returned by makeCacheMatrix
## If inverse has already been claculated, cacheSolve will retrieve inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()   
    if(!is.null(inv)) {                 ## if inverse matrix is not NULL
        message("getting cached data")  ## Print Message
        return(inv)                     ## return invertible matrix
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    return(inv)
}


### Test
TestMatrix <-  matrix(c(1,5,8,2),2,2)
TestMatrix
CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$get()
CacheMatrix$getinverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)

CacheMatrix$getinverse()