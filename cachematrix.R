##This function creates a special "matrix" object that can cache its inverse
##The first function, makeCacheMatrix does the following 
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse matrix
##4.get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) { 
     i <- NULL 
     set <- function(y) { 
         x <<- y 
         i <<- NULL 
     } 
     get <- function() x 
     setmtx <- function(mtx) i <<- mtx 
     getmtx <- function() i 
     list(set=set, get=get, setmtx=setmtx, getmtx=getmtx) 
 } 

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

 cacheSolve <- function(x, ...) { 
     i <- x$getmtx() 
     if(!is.null(i)) { 
         message("getting cached data") 
         return(i) 
     } 
     data <- x$get() 
     i <- solve(data) 
     x$setmtx(i) 
     i 
 } 

## Taken from the example indicated in the assignment
