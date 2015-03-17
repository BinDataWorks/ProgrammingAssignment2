##################################################################
# FUNCTION: makeCacheMatrix                                      
##################################################################
# This function creates a special "matrix" object 
# that can cache its inverse.
# Sample matrix 1 (that also has an inverse): 
#               mdat <- matrix(c(1.00 ,-0.25 ,-0.25  , 1.00), 
#                               nrow = 2, ncol = 2)
#               The above matrix would produce the result below:
#                 [,1]  [,2]
#                 [1,]  1.00 -0.25
#                 [2,] -0.25  1.00
# Sample matrix 2  (that also has an inverse;
#                   this is actually the inverse of the above): 
#               mdat <- matrix(c(1.0666667, 0.2666667, 
#                               0.2666667, 1.0666667), 
#                               nrow = 2, ncol = 2)
#               The above matrix would produce the result below:
#                 [,1]      [,2]
#                 [1,] 1.0666667 0.2666667
#                 [2,] 0.2666667 1.0666667
##################################################################

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##################################################################
# FUNCTION: cacheSolve                                      
##################################################################
# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), then the 
# cachesolve should retrieve the inverse from the cache.
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

# ####################################################################
# # TEST OF ABOVE FUNCTIONS (uncomment below next line to use)                                     
# ####################################################################
# mdat <- matrix(c(1.00 ,-0.25 ,-0.25  , 1.00), nrow = 2, ncol = 2)
# ################################
# # EXAMPLE 1: using cached matrix
# ################################
# samplem<-makeCacheMatrix(mdat)
# samplem$getinverse() # should return NULL
# inversemdat<-solve(mdat) # return inverse of mdat
# samplem$setinverse(inversemdat) # cache mdat
# samplem$getinverse() # should return inverse of mdat
# cacheSolve(samplem) # return cached inverse of mdat
# ####################################
# # EXAMPLE 2: not using cached matrix
# ####################################
# samplem2<-makeCacheMatrix(mdat)
# samplem2$getinverse() # should return null
# cacheSolve(samplem2) # should return un-cached inverse of mdat

