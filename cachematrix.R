## makeCacheMatrix: This function creates a special "matrix" object that can cache its  
## inverse.

makeCacheMatrix <- function(m = matrix()) 
{
    i <- NULL
    set <- function(y) 
{
      m <<- y
      i <<- NULL
    }
    get <- function() m
    setInv <- function(solve) i <<- solve
    getInv <- function() i
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## `cacheSolve` function computes the inverse of matrices returned by `makeCacheMatrix` 
cacheSolve <- function(m, ...) 
{ 
    i<- m$getInv()
    if(!is.null(i)) 
{
      message("Retrieving cached data")
      return(i)
    }
    data <- m$get()
    i<- solve(data, ...)
    m$setInv(i)
    i
}
