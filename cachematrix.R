## Put comments here that give an overall description of what your
## functions do


#cachematrix.R----------------done by A M Rajee
        
##This function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
#get the value of the matrix, set the inverse matrix and get the inverse matrix.  The matrix object
#can cache its own object.

#<<- operator is used to assign a value to an object in an environment that is different
#from current enviornment.
        
#take the matrix as an input
makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL

#set the value of the matrix
setMatrix <- function(y)  {
        x <<- y
        invMatrix <<- NULL
}
        
getMatrix <- function() x                                        #get the value of the matrix
setInverse <- function(inverse)  invMatrix <<- inverse           #set the value of the invertible matrix
getInverse <- function()                                         #get the value of the invertible matrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
#get the value of the invertible matrix from the makeCacheMatrix function
invMatrix <- x$getInverse()
if(!is.null(invMatrix)) {                               #if inverse matrix is not NULL                          
        message("Getting Cache Invertible Matrix")      #Type Message: Getting Cached Invertible Matrix
        return(invMatrix)                               #Return the Invertible Matrix
 }
        
 #if value of the invertible matrix is NULL then
 MatrixData <- x$getMatrix()                            #Get the original Matrix Data
 invMatrix <- solve(MatrixData, ...)                    #Use solve function to inverse the matrix
 x$setInverse(invMatrix)                                #set the Invertible Matrix
 return(invMatrix)                                      #return the Invertible Matrix
        
  }



#########Testing##########

####Test 1 [2*2 Matrix] #####
TestMatrix <- matrix(1:4,2,2)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)


####Test 2 [2*2 Matrix] #####
TestMatrix <- matrix(c(1,5,8,2),2,2)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)


####Test 3 [3*3 Matrix]#####
TestMatrix <- matrix(1:8,3,3)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)


####Test 4 [4*4 Matrix]#####
TestMatrix <- matrix(c(2,3,5,1,3,7,4,5,6,8,0,0,4,5,6,0),4,4)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)

####Test 5 [3*3 Matrix] --> singular Matrix #####
#matrix(1:9,3,3) is not possible (singule matrix) because it is giving det(A)  = 0
#matrix = 1/det(A)[3,-6,3,-6,12,-6,3,-6,3] |det(A) = 1/(1*3 +4* (-6) + 7 *3) = 1/0
TestMatrix <- matrix(1:9,3,3)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)

####Test 6 [4*4 Matrix]  --> singular Matrix #####
TestMatrix <- matrix(5:21,4,4)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)


