## Matrix inversion is usually a costly computation
## There is a benefit to caching the matrix inverse than repeatedly recomputing it 
## The following two functions are used to cache the inverse of a matrix.
##
## makeCacheMatrix uses the same steps as the caching example in the problem description
##
## it makes a list containing a function and
## uses the <<- operator to store values outside the lexical scoping environment
##
## It Sets the value of the matrix, gets the value of the matrix
## It Sets the value of the matrix inverse, gets the value of the matrix inverse
 

makeCacheMatrix <- function(x = matrix()) {
    
    # matrixinv is the name of the cache to store the inverse 
    matrixinv <- NULL
    
    # sets the matrix and uses the <<- operator to avoid lexical scoping rules
    set <- function(y) {
        x <<- y
        matrixinv <<- NULL
    }
    
    # gets the matrix
    get <- function() x
    
    
    # sets the inverse
    setinverse <- function(inverse) matrixinv <<- inverse
    
    # gets the inverse
    getinverse <- function() matrixinv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##
## cacheSolve returns the inverse of a matrix
## It assumes a matrix is square and invertible and computes the inverse by:
## 
## Checking if the inverse has already been computed and the matrix hasn't changed
## 
## If no change, gets the previous computation from the cache/skips new calculation
## 
## If the matrix has changed, it computes the inverse, and caches the new value
## 


cacheSolve <- function(x, ...) {
	matrixinv <- x$getinverse()
    
    # checks if already calculated and, if so, retireves from the cache 
    if(!is.null(matrixinv)) {
        message("getting cached data.")
        return(matrixinv)
    }
    
    # if not, then calculate the inverse using the matrix solve command
    data <- x$get()
    matrixinv <- solve(data,...)
    
    # Cache the newly calculated matrix inverse for future use
    x$setinverse(matrixinv)
    
    # Return the value of the inverse
    matrixinv
}
