## This pair of functions cache the inverse of a matrix

## The first function creates a special "matrix" 
## object that can cache its inverse

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
        
        getinv <- function(){    ## Get the inverse of the matrix
                inv
        }
        
        list(set=set, get=get, setinv=setinv, getinv=getinv)
        ## return a list with the functions that set and get the matrix
        ## and its inverse
        
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        
        if(!is.null(inv)){           ## It returns the inverse 
                                     ##if it has already been calculated
                
                message("getting cached data")
                return(inv)
        }
        
        ## It continues evaluating only if the inverse has not been calculated
        
        data <- x$get()  ## It gets the matrix and saves it in data
        
        inv <- solve(data,...) ## It calculates the inverse 
                               ## of data and saves it in inv
        
        x$setinv(inv) ## It sets the inverser in the "matrix" x
        
        inv  ## It returns the inverse of x
}
