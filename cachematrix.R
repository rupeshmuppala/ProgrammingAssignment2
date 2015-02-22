# This function makes the matrix for which we want to calculate inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # Initializing inverseMatrix to NULL for the firstime
    inverseMatrix<-NULL
    
    #setting the matrix to x and resetting the inverseMatrix to remove the cached version
    #we are using << as the changes we make here should refelct in other environments 
            set <- function(y) {
              x <<- y
              inverseMatrix <<- NULL
            }  
    
            get <- function() x
    
    #setInverseMatrix call from cacheSolve should have the changes in that environment
    #so we are using << here
            setInverseMatrix <- function(inverse) inverseMatrix <<- inverse
    
    
            getInverseMatrix <- function() inverseMatrix
    
            list(set = set, get = get,
                 setInverseMatrix = setInverseMatrix,
                 getInverseMatrix = getInverseMatrix)

}


#This function calculates the inverse of matrix through solve function for the
#first time and returns the cached version for subsequent function calls.

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
              inverseMatrix <- x$getInverseMatrix()
         # return the cached version if the result is already cached     
              if(!is.null(inverseMatrix)) 
                {
                message("getting cached data")
                return(inverseMatrix)
                }
         
         # calculate the inverse of a matrix for the first time
              data <- x$get()
              inverseMatrix <- solve(data, ...)
              x$setInverseMatrix(inverseMatrix)
              inverseMatrix
  
}
