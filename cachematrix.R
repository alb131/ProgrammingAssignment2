## Together these functions compute and cache the inverse of a matrix

## Creates a list containing functions to
##		1. set the matrix object
##		2. get the matrix object
##		3. set the inverse of the matrix
##		4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 	inv <- NULL				
     set <- function(y) {
     	x <<- y			
    		inv <<- NULL		
   	}
   	get <- function() x 					
 	setinverse <- function(inverse) inv <<- inverse		
     getinverse <- function() inv				
     list(set = set, get = get,				
     	setinverse = setinverse,
    	getinverse = getinverse)
}


## Computes the inverse of a matrix returned by makeCacheMatrix if it has not been previously calculated. If the matrix inverse has been previously calculated, this function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
         inv <- x$getinverse()
        if(!is.null(inv)) {					
                message("getting cached data")
                return(inv)					
        }
        data <- x$get()						
        inv <- solve(data, ...)					
        x$setinverse(inv)						
        inv	
}
