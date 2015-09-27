## These functions are designed to cache the inverse of a 
## matrix (assumed to be square and invertible). The purpose
## is to avoid repeating unnecessariy the costly computation
## of matrix conversion. There are two functions involved.



## This function creates a special "matrix" object that 
## can cache its inverse. This is actually a list of four 
## functions to
## 1. Set the matrix whose inverse we seek (& clear inverse)
## 2. Get the matrix which has been set
## 3. Set (create) the inverse of this matrix
## 4. Get the inverse which has been created

makeCacheMatrix <- function(x = matrix()) {
       	mtx <- NULL
 
	## create the 4 functions
	   	set <- function(y) {
                x <<- y
                mtx <<- NULL
        	}
        	get <- function() x
        	setinverse <- function(minverse) mtx <<- minverse
        	getinverse <- function() mtx
        
	## return the functions in a list
		list(	set = set, get = get,
           	setinverse = setinverse,
             	getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" ## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (for this matrix), then cacheSolve ## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        
	## retrieve what is cached in the inverse matrix store
	mtx <- x$getinverse()
        
	## if it exists, return that inverse matrix
	if(!is.null(mtx)) {
                message("getting cached data")
                return(mtx)
    	}
        
	## if it does not exist, get the matrix whose inverse we 	## seek and compute, store and return its inverse
	data <- x$get()
    	mtx <- solve(data, ...)
     	x$setinverse(mtx)
  	mtx
}
