##################################################################
# PLEASE NOTE:  
#       1) THIS SCRIPT ASSUMES THAT A MATRIX (PASSED AS A
#       PARAMETER) WILL ALSO HAVE AN INVERSE.
#
#       2) Although some examples have been provided below, it
#       is recommended that you view the accompanying file
#       "MatrixSolutionExamples.md" IF you would like to
#       look at additional usage examples.
##################################################################

##################################################################
# 1st of 2 FUNCTIONS in this script: makeCacheMatrix                                      
##################################################################
# DESCRIPTION:  This function creates a special "matrix" object 
#               that can cache its inverse. This function caches
#               a matrix and its inverse but does not calculate
#               the inverse. Please look at the function
#               cacheSolve if you wish to solve for an inverse.
#
# EXAMPLE:    
#       A EXAMPLE use of this function follows:
#-----------------------------------------------------------------
#       mdat <- matrix(c(1.00 ,-0.25 ,-0.25  , 1.00), 
#                               nrow = 2, ncol = 2)
#       inversemdat<-solve(mdat)
#       
#       samplem<-makeCacheMatrix(mdat) # stores mdat
#       samplem$getinverse() # returns inverse of mdat if 
#                            # previously set
#       samplem$setinverse(inversemdat) # stores inverse of mdat
#-----------------------------------------------------------------
##################################################################

makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL
        # function: to set the value of the matrix
        set <- function(y) {
                m <<- y
                inv <<- NULL
        }
        # function: to get the value of the matrix        
        get <- function() m
        # function: to be used to set the inverse of the matrix        
        setinverse <- function(m) inv <<- m
        # function: to be used to get the inverse of the matrix                
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##################################################################
# 2nd of 2 FUNCTIONS in this script: cacheSolve                                      
##################################################################
# DESCRIPTION:  This function computes the inverse of 
#               the special "matrix" returned by makeCacheMatrix 
#               above. If the inverse has already been calculated 
#               (and the matrix has not changed), then the
#               cacheSolve function should retrieve the 
#               inverse from the cache.
#
# EXAMPLE:    
#       An EXAMPLE use of this function follows:
##----------------------------------------------------------------
#       samplem2<-makeCacheMatrix(mdat)
#
#       cacheSolve(samplem2) # should return cached inverse of 
#                            # mdat (if previously set in sample2); 
#                            # otherwise, it will will solve for 
#                            # the inverse of mdat
##----------------------------------------------------------------
##################################################################

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        
        # check to see if the matrix has already been set; 
        # if so, return the cached inverse
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        data <- x$get()
        
        # if the cached inverse (described above) doesn't exist,
        # calculate and return the inverse of the matrix
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
