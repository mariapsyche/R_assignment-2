## These functions calculate the inverse of a matrix if it has not
## been calculated, or retrieve the result if it has been
## calculated before. Hence it may run much faster than mandatory 
## matrix inversions, especially when many of these are required.

## This function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  xinv<-NULL
  set<-function(y){
    x<<-y
    xinv<<-NULL
  }
  get<-function() x
  setinv<-function(solve) xinv<<-solve
  getinv<-function() xinv
  list(set=set,get=get,
       setinv=setinv,getinv=getinv)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix, or retrieve its inverse if the 
## this calculation was completed before.

cacheSolve <- function(x, ...) {
  xinv<-x$getinv()
  if(!is.null(xinv)){
    message("getting cached data")
    return(xinv)
  }
  data<-x$get()
  xinv<-solve(data,...)
  x$setinv(xinv)
  xinv
}
