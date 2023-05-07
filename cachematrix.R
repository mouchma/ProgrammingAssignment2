## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #set value of variable representing inverse the matrix object to null 
  #store it in cache
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  #create the four functions that will be embedded within the object created by
  #the makeCacheMatrix function call
  
  #set up the matrix
  get <- function() x
  #set up the function to send the inverse to cache
  setinvrs <- function(inverse) invrs <<- inverse
  #get the inverse of the object from cache
  getinvrs <- function() invrs
  
  #return a list of the functions that will be accessible to the matrix object
  list(set = set, get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #check if an inverse for the matrix is stored
  #if it is, return the stored inverse matrix
  invrs<-x$getinvrs
  if (!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  # if an inverse for the matrix is not stored
  # call solve to get the inverse and use set to store it in cache
  else{
    invrs <- solve(x$get(),...)
    x$setinvrs(invrs)
  }
}
