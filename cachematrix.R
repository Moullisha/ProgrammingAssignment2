## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix() takes a matrix as input and returns a list of functions
## It contains 4 functions which are- set() which is used to assign a valye y to x
## get() which is usedto get the value of x
## setInverse() is used to set the value of inv(inverse of a matrix)
## getInverse() is used to get the value of inv

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv= NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setInverse<-function(solve) inv<<-solve
  getInverse<-function() inv
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## Write a short comment describing this function
## This function checks whether or not the inverse of the matrix has already been 
## stored in the memory or not.
## If yes, it prevents the recomputation and returns the stored value of inverse
## In case the user has entered a new matrix whose inverse has not been calculated,
## the function then calculates the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setInverse(inv)
  inv
        
}
