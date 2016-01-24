## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## This function creates a special "matrix" object that can cache its inverse

# makeCacheMatrix creates a list containing a function to do following 4 steps
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    	set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    	get <- function() x
    	setinverse <- function(inverse) inv <<- inverse
    	getinverse <- function() inv
    	list(set=set, 
	     get=get, 
	     setinverse=setinverse, 
	     getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
    		if(!is.null(inv)) {
        	message("getting cached data.")
        	return(inv)
    }
    	data <- x$get()
    	inv <- solve(data)
    	x$setinverse(inv)
    	inv
}

## Result of the above 2 functions
## > x = rbind(c(1, -1/5), c(-1/5, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
## [1,]  1.0 -0.2
## [2,] -0.2  1.0
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0416667 0.2083333
## [2,] 0.2083333 1.0416667
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0416667 0.2083333
## [2,] 0.2083333 1.0416667
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0416667 0.2083333
## [2,] 0.2083333 1.0416667
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 
## 