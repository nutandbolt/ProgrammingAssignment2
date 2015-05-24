## This function outputs a list of 4 functions
## "get function" outputs the stored matrix
## "set function" inputs a new matrix
## "setinverse" stores the inverse of the matrix to be recalled later
## "getinverse" outputs the inverse of the matrix stored in the variable inverse

## 

makeCacheMatrix <- function(x = matrix()) {
  
  inverse<-NULL
  
  ## Function to set the input matrix
      set<-function(y=matrix()){
    
        x<<-y
    
        inverse <<- NULL
      }
  ## Function to output the matrix
      get <- function() x
  ## Function to set the inverse 
      setinverse <-function(inverse_matrix) inverse <<- inverse_matrix
  ## Function to output the inverse
      getinverse <-function() inverse
  ## List to output all the 4 functions
      list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## THIS FUNCTION GETS THE INVERSE OF THE MATRIX BY USING THE FUNCTION SOLVE IN CASE THE
## INVERSE IS NOT FOUND IN THE VARIABLE "inverse"

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <-x$getinverse()
        ## Check if the inverse has been cached 
  if (!is.null(inverse)) {
    message("getting cached data")
    return (inverse)
  }
  data <-x$get()
      ## Use the built in function solve to get the inverse
  inverse <-solve(data)
  x$setinverse(inverse)
  inverse
}

