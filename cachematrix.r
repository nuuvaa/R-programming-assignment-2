## This pair of functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its
##inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	set <- function(y){  ## We set the matrix
		x <<- y
		inv <<- NULL
	}
	
	get <- function(){   ## Get the matrix
		x
	}
		
	setinv <- function(inverse){  ##  Set the inverse of the matrix
		inv <<- solve(inverse)
	}
	
	getinv <- function(){
		inv
	}
	
	list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## This function computes the inverse of the special "matrix" returned
##by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve should retrieve
##the inverse from the cache.

cacheSolve <- function(x, ...) {
	
	inv <- x$getinv()
	
	if(!is.null(inv)){  ##it returns the inverse if it has already been
						##calculated
		message("getting cached data")
		return(inv)
	}
	
	data <- x$get()
	
	inv <- solve(data,...)
	
	x$setinv(inv)
	
	inv
}
