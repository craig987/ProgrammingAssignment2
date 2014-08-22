## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix sets up three functions that are later used by cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  
      # mcheck is the check that the Inverse has been calculated
      # mcheck is initially set to null
      
      mcheck<-NULL
      set<-function(y){
                  x<<-y
                  mcheck<<-NULL
      }
      get<-function() x
      setmatrix<-function(solve) mcheck<<- solve
      getmatrix<-function() mcheck
      list(set=set, get=get,
           setmatrix=setmatrix,
           getmatrix=getmatrix)
      }
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         mcheck<-x$getmatrix()
      
      ## This is the test to see if there is cached data (mcheck not null)
      
      if(!is.null(mcheck)){
            message("getting cached data")
            return(mcheck)
      }
      matrix<-x$get()
      mcheck<-solve(matrix, ...)
      x$setmatrix(mcheck)
      mcheck
        
}
