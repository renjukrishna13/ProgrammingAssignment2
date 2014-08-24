## This is done as a part of R Programming Assignment2
## This file contains two functions makeCacheMatrix and
## cacheSolve . Since matrix inversion is a costly 
## computation, these two functions are helpful to
## cache the inverse of a matrix rather than computing
## repeatedly.

## This function will create a "matrix" object that
## can cache its inverse. The argument for this function
## is a matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # inv will store our inverse of the matrix and it
                    # is reset to NULL every time makeCacheMatrix() 
                    # is called.
        
        set <- function(y){  # This function takes an input matrix.
                x <<- y      # Stores the input matrix
                inv <<- NULL # resets the inv to NULL
        }
        
        get <- function() { x } # this function returns the value of 
                                # the original matrix.
        
        setinverse<- function(inverse){ # This is called by CacheSolve() during 
                                        # first CahceSolve() call and it 
                                        # will store the value using superassignment
                inv <<- inverse
        }
        
        getinverse <- function(){inv}   # This will return the cached value 
                                        # to cacheSolve() on subsequenct calls.
        
        list(get=get,setinverse=setinverse,getinverse=getinverse) # This list which is 
                               # newly created is an object containg the methods which 
                               # can be called on an object created by this function.
                               # This list get returned when the makeCacheMatrix is called.
}


## This function when called to get the inverse of a matrix will calculate 
## the inverse and return if the inverse has not been calculated before. 
## If it has already been calulated,it will return the cached value.

cacheSolve <- function(x, ...) { # the input is an object created by makeCacheMatrix
        inv <- x$getinverse()    # accesses the object 'x' and gets the value of the inverse
        
        if(!is.null(inv)) {      # if inverse was already cached (not NULL) ...
                message("getting cached data")  # print message to the console
                return(inv)                     # ... and return the inverse ... "return" ends 
                                                #   the function cacheSolve()
        }
        
        data <- x$get()        # This is executed only if x$getinverse() returned NULL
        inv <- solve(data)     # if inv was NULL then we have to calculate the inverse
        x$setinverse(inv)      # store the calculated inverse value in x
        inv                    # return the inv to the code that called this function
}
