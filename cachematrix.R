## Put comments here that give an overall description of what your
## functions do

## A set of functions that cache the inverse of a matrix

##Creates a special matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()){
  
  ## Initializes the inverse
  i <- NULL 
  
  ## Sets the matrix
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  ## Obtains the matrix
  get <- function() 
    
    ## Returns the matrix
    x
  
  ## Sets the inverse of the matrix
  setInverse <- function(inverse) 
    i <<- inverse
  
  ## Obtains the inverse of the matrix
  getInverse <- function()  
    
    ## Returns the inverse
    i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

## Computes the inverse of the special matrix created by makeCacheMatrix

## If the inverse has been calculated, cacheSolve will return cache data
cacheSolve <- function(x, ...){
  
  ## Returns the inverse matrix of x
  i <- x$getInverse()
  
  ## Returns the inverse if it is already set
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  ## Obtains the matrix
  mat <- x$get()
  
  ## Calculates the inverse 
  i <- solve(mat, ...)
  
  ## Sets the inverse 
  x$setInverse(i) 
  
  ## Returns a matrix that is the inverse of x
  i
}
