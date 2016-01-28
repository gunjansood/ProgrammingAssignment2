## makeCacheMatrix creates a special "matrix" ,which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x=matrix())
{
  inv <-NULL
  set <- function(y){
    x<<-y
    inv <<-NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv<<-inverse
  getInverse <- function() inv
  list(set = set , get =get, setInverse = setInverse, getInverse = getInverse)
}

## The following function computes the inverse of the special "matrix" created by makeCacheMatrix above.
## It first checks if the inverse has already been calculated 
## then it retrieves the inverse from the cache. Otherwise computes the inverse
## of the matrix and sets the value of the inverse via setInverse function.

cacheSolve <- function(x,...)
{
  inv <- x$getInverse()
  if (!is.null(inv))
  {
    message("getting cached matrix")
    return(inv)
  }
  mtr <- x$get()
  inv <- solve(mtr,...)
  x$setInverse(inv)
  inv
}