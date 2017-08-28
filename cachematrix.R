##The following functions work together 
##to cache the inverse of a matrix


##The first function "makeCacheMatrix" 
##creates a list containing a function to
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the Inverse
##4.get the value of the Inverse
makeCacheMatrix<-function(x=matrix())
{   m<-NULL
    set <- function(y) {
         x<<-y
         m<<-NULL
    }
    get<-function()x
    setInverse<-function(inverse)
        {m<<-inverse}
    getInverse<-function()m
    list<-list(set=set,get=get,
           setInverse=setInverse,
           getInverse=getInverse)
}
##The following function calculates 
##the inverse of the special "matrix" 
##created with the above function.
##If the inverse has already been calculated,
##it gets the inverse from the cache 
##and skips the computation. 
cacheSolve<-function(x, ...)
{
    ## Return a matrix that is 
    ## the inverse of 'x'  
    m <-x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
#TEST
A<-matrix(c(1,2,3,4),2,2)
A1<-makeCacheMatrix(A)
cacheSolve(A1)