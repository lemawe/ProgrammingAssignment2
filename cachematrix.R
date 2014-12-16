## Put comments here that give an overall description of what your
## functions do

## This function initializes the object that will contains
## a matrix and his corresponding inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverse=NULL  #This is the inverse of the matrix, reset to Null
                #every time the function is called to create or modify an object
  
  
  #The functions below are setter and getter. There are required to access or
  ## initiate any properties of the object.
  
  set = function(y){  
    x<<-y
    inverse<<-NULL
  }
  get = function(){x}  
  
  setInv = function(inv) inverse<<-inv
  
  getInv = function()inverse 
  
  #List of internal functions to inform a calling function how to access those methods
  list(set=set,get=get, setInv=setInv, getInv= getInv)

}


## This function returns a matrix that is the inverse of 
## a matrix contained in an object x

cacheSolve <- function(x, ...) {
  
  #We try to get the inverse of the matrix.
  result = x$getInv()
  
  #If the result is no null, we take the data from the cache
  if(!is.null(result)){
    message("getting data from cache")
    return (result)
  }
  
  #If the result is null, we compute the inverse of the matrix
  inverse = solve(x$get())
  
  #We set the inverse property of the object, so if the object does not change,
  #we do not need to recalculate the inverse.
  result = x$setInv(inverse)
  message("compute new inverse")
  result  
}
