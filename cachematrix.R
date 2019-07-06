## Caching the Inverse of a Matrix:
## Matrix inversion is a costly computation and there is some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  setMat<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  getMat<-function() x
  setInv<-function(inverse) inv <<- inverse
  getInv<-function() inv
  list(setMat=setMat, getMat=getMat, setInv=setInv, getInv=getInv)
}


## This function computes the inverse of the matrix created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
    message("Getting cached inverse Matrix")
    return(inv)
  }
  mat <- x$getMat()
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv
  
}
