##################################################################
# PLEASE NOTE:  1) THIS SCRIPT ASSUMES THAT A MATRIX (PASSED AS A
#               PARAMETER) WILL ALSO HAVE AN INVERSE.
#               2) Although some examples have been provided, it
#               is recommended that you view accompanying file
#               "MatrixSolutionExamples.md" for additional usage
#               examples.
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
        set <- function(y) {
                m <<- y
                inv <<- NULL
        }
        get <- function() m
        setinverse <- function(m) inv <<- m
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

# ##########################################################################
# # ONE SUGGESTED TEST OF ABOVE FUNCTIONS (uncomment below next line to use)                                     
# ##########################################################################
## 1) Assign a matrix to mdat:
##------------------------------------------------------------------------
#       mdat <- matrix(c(1.00 ,-0.25 ,-0.25  , 1.00), nrow = 2, ncol = 2)
##------------------------------------------------------------------------
# ################################
# # EXAMPLE 1: using cached matrix
# ################################
## 2) If using a cached matrix, try the following:
##       a) assign the value of mdat to samplem:
##------------------------------------------------------------------------
#               samplem<-makeCacheMatrix(mdat)
##------------------------------------------------------------------------
##       b) the next step isn't necessary, but calling samplem's getinverse 
##          function should return a NULL value:
##------------------------------------------------------------------------
#               samplem$getinverse() # should return NULL
##------------------------------------------------------------------------
##       c) Since the inverse isn't actually being calculated in the function 
##          makeCacheMatrix (it is only stored), we find (and then store) 
##          the inverse of mdat into inversemdat:
##------------------------------------------------------------------------
#               inversemdat<-solve(mdat) # return inverse of mdat
##------------------------------------------------------------------------
##       d) We can then use the setinverse function to store the inverse 
##          that was determined above:
##------------------------------------------------------------------------
#               samplem$setinverse(inversemdat) # cache mdat
##------------------------------------------------------------------------
##       e) Now, if one calls the getinverse function, one shouldn't get
##          a NULL value; one should get the inverse of the matrix
##------------------------------------------------------------------------
#               samplem$getinverse() # should return inverse of mdat
##------------------------------------------------------------------------
##       f) So if one calls the cacheSolve function, we should get the
##          cached matrix stored above via setinverse
##------------------------------------------------------------------------
#               cacheSolve(samplem) # return cached inverse of mdat
##------------------------------------------------------------------------
# ####################################
# # EXAMPLE 2: not using cached matrix
# ####################################
## 3) If NOT using a cached matrix, try the following:
##      a) assign the value of mdat to samplem2:
##------------------------------------------------------------------------
#       samplem2<-makeCacheMatrix(mdat)
##------------------------------------------------------------------------
##       b) as in the case of Example 1 above, the initial call to
##          getinverse function should return a NULL value
##------------------------------------------------------------------------
#       samplem2$getinverse() # should return null
##------------------------------------------------------------------------
##      c) calling cacheSolve should now return the un-cached inverse of
##         mdat
##------------------------------------------------------------------------
#       cacheSolve(samplem2) # should return un-cached inverse of mdat
##------------------------------------------------------------------------

