 
# Examples with Both makeCacheMatrix and cacheSolve functions

 
First, assign a matrix to mdat:

      mdat <- matrix(c(1.00 ,-0.25 ,-0.25  , 1.00), nrow = 2, ncol = 2)

 
## EXAMPLE 1: using cached matrix
 
###If using a cached matrix, try the following:
Assign the value of mdat to samplem:

      samplem<-makeCacheMatrix(mdat)

The next step isn't necessary, but calling samplem's getinverse function should return a NULL value:

      samplem$getinverse()

Since the inverse isn't actually being calculated in the function makeCacheMatrix (it is only stored), we find (and then store) the inverse of mdat into inversemdat:

      inversemdat<-solve(mdat)
      
We can then use the setinverse function to store the inverse that was determined above:

      samplem$setinverse(inversemdat)

Now, if one calls the getinverse function, one shouldn't get a NULL value; one should get the inverse of the matrix

      samplem$getinverse()

So if one calls the cacheSolve function, we should get the cached matrix stored above via setinverse

      cacheSolve(samplem)

 
## EXAMPLE 2: not using cached matrix
 
### If NOT using a cached matrix, try the following:
Assign the value of mdat to samplem2:

      samplem2<-makeCacheMatrix(mdat)

As in the case of Example 1 above, the initial call to getinverse function should return a NULL value:

      samplem2$getinverse()

Calling cacheSolve should now return the un-cached inverse of mdat:

      cacheSolve(samplem2)


<i>(From BinDataWorks for R Programming -- Programming Assignment #2, March 2015)</i>