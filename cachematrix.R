## This file contains two functions for creating a "special" matrix that is capable of storing/caching its inverse
## - makeCacheMatrix
## - cacheSolve

## makeCacheMatrix: function that makes a special matrix object capable of storing a cached inverse matrix
makeCacheMatrix <- function(x = matrix())
{
  # set cache variable to NULL when special matrix is created
  inverse <- NULL
  
  # functions for setting and getting the actual matrix, and setting and getting the cached inverse
  set <- function(m)
  {
    x <<- m
    # reset the cached inverse since we have a new matrix now
    inverse <<- NULL
  }
  get <- function()
  {
    x
  } 
  getinverse <- function()
  {
    inverse
  }
  setinverse <- function(i)
  {
    inverse <<- i
  }
  
  # return a list of the functions
  list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
  
}

## cacheSolve: solves the supplied "special" matrix unless a cached inverse is already set in the matrix object
cacheSolve <- function(x, ...) {
  
  # First check if an inverse is already set
  i <- x$getinverse()
  if(!is.null(i))
  {
    # inverse is set for the matrix, so lets reuse this cached value
    print("Using cached value:")
    return(i)
  } else
  {
    # inverse is not set for the matrix, so lets solve it
    print("No cached value, solving:")
    s = solve(x$get())
    x$setinverse(s)
    return(s)
  }
}

