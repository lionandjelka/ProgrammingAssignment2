## following functions  cache the inverse of a matrix:
## first function creates a special "matrix" object that can cache its inverse.
## second function computes the inverse of the special "matrix" returned by  the first function 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
##  now creates inverse matrix which is filled with NULL's 
  dm=dim(x)
  create.inv <- function(dm1) {
    
    s=dm1[1]
    p <- list()
    length(p) <- s^2
    dim(p) <- c(s,s)
    p
  }
  inv.mat<-create.inv(dm)
  
  
##setting values of input matrix and inverse
 
  set <- function(y) {
    x <<- y
    inv.mat<<-create.inv(dm)
  }
## getting values of input matrix and inverse
  get <- function() x
## setting a value of solve function  
  setsolve <- function(solve) inv.mat <<- solve
## getting a value of solve function 
  getsolve<- function() inv.mat
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}






## This function computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv.mat <- x$getsolve()
 
#testing existence of invertted matrix
  q<-!sapply(inv.mat, is.null)

  if(q[1]) {
   message("getting cached data")
   return(inv.mat)
  }
#if inverted matrix does not exist then calculate it via setsolve function
  data <- x$get()
  inv.mat <- solve(data, ...)
  x$setsolve(inv.mat)
  inv.mat
}



