## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly. 
## Two functions must used combined and used in sequence
## e.g. of test :
## > source("cachematrix.R")
## >M<-matrix(rnorm(100,0,1),10,10)
## >M_list<-makeCacheMatrix(M)
## >cacheSolve(M_list)
## > ....inverse matrix printout
## >cacheSolve(M_list)
## > getting cached matrix inverted
## > ....inverse matrix printout


## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## input: numeric matrix that MUSt be invertible (det !=0)
## output: list of the cached matrix with its enviromental
makeCacheMatrix <- function(X = matrix()) {
  Xinv <- NULL
  set <- function(Y) {
    X <<- Y
    Xinv <<- NULL
  }
  get <- function() X
  setinv <- function(inv) Xinv <<- inv
  getinv <- function() Xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from 
## the cache.
## input : list coming from from makecacheMatrix
## output: inverse matrix


cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'x'
  Xinv <- X$getinv()
  if(!is.null(Xinv)) {
    message("getting cached matrix inverted")
    return(Xinv)
  }
  data <- X$get()
  Xinv <- solve(data, ...)
  X$setinv(Xinv)
  Xinv
}