## This script consists of two functions that cache the inverse of a matrix, such that it doesn't 
## need to be calculated repeatedly

## The first function creates a special matrix object that can cache its inverse. For this purpose, 
## it needs a squared matrix that could be inverted as argument. First the object "inv" is created. 
## The function then involves a nested function "set", which is able to overwrite the matrix x with 
## the values of a different matrix, y (from outside the function). The object "get" can be called 
## to show the values of the matrix x. Object "setinv" can be called to assign values of the inverse 
## of the matrix (from outside) to "inv". "getinv" can be called to show the values of the inverse 
## of matrix x. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) { 
    x <<- y 
    inv <<- NULL 
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# Below on line 26 an example with an invertible matrix:
# a <- makeCacheMatrix(matrix(sample(1:20,9),3,3))


## The second function computes the inverse of the special matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the function 
## cacheSolve retrieves the inverse from the cache. The function uses the first function as argument 
## and first looks if the inverse for this matrix has already been calculated. If it was already 
## calculated, the function retrieves the inverse from the cashe and returns the message "getting 
## cashed data" together with the retrieved inverse. Otherwise the function calculates the inverse
## of the matrix x and in the end returns it. 


cacheSolve <- function(x, ...) {
  inv <- x$getinv() 
  if(!is.null(inv)) { 
    message("getting cached data") 
    return(inv) 
  }
  data <- x$get() 
  inv <- solve(data, ...) 
  x$setinv(inv) 
  inv
}

# cacheSolve(a)
