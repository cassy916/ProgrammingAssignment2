##Function 1: Make a special matrix than can be cached


makeCacheMatrix <- function(x = matrix()) {
  ##Initialize NULL
  inv <- NULL
  ##Set Matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  ##Get Matrix
  get <- function()x
  
  ##Set inverse
  setInverse <- function(inverse) inv <<- inverse
  
  ##Get inverse
  getInverse <- function() inv 
  
  ##Return list 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

##Function 2: return inversed matrix 

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  
  ##Return inv if already calculated 
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  ##Get matrix
  mat <- x$get()
  
  ##Calculate inverse 
  inv <- solve(mat,...)
  
  ##Set inverse
  x$setInverse(inv)
  
  ##Return the matrix 
  inv