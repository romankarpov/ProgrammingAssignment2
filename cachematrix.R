## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix generates a list of functions to maintain cached value of the inverted (solved) matrix

makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL ;
  set <- function(y){
    
    x <<- y ;
    s <<- NULL 
    
  }
  
  get <- function() x
  setSolve <- function(solve) s<<- solve
  getSolve <- function() s 
  list(get = get, set = set, getSolve = getSolve, setSolve = setSolve)
  
}


## cacheSolve retrieves inverted matrix either from cache or calculates the inverted matrix and saves it inot 
## cache if the cache is empty 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x
  r<-x$getSolve()
  if (!is.null(r)){
    message("Retrieving cahced data")
    return(r)
  } else {
    r<-solve(x$get(), ...) ;
    x$setSolve(r) ;
    r 
  }
}