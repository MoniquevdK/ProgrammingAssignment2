## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # inverse moet nog berekend worden
  set <- function(y) { # om matrix x evt te overschrijven met nieuwe data
    x <<- y # matrix x wordt overschreven met matrix y van buitenaf
    inv <<- NULL # inv moet (opnieuw) berekend worden
  }
  get <- function() x # kun je aanroepen om matrix x te kunnen zien
  setinv <- function(inverse) inv <<- solve # geef evt een waarde van inv van buitenaf
  getinv <- function() inv # laat zien wat inv is
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

a <- makeCacheMatrix(matrix(sample(1:20,9),3,3))
a$get()
a$getinv()



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinv() # bekijk of er een een waarde van m aanwezig en zo ja wat deze dan is
  if(!is.null(inv)) { # indien er al een waarde is
    message("getting cached data") # geef dan deze tekst weer
    return(inv) # en ook de waarde van m
  }
  data <- x$get() # bekijk de vector 
  inv <- solve(data, ...) # bereken de mean van vector x
  x$setinv(inv) #  # geef berekende waarde van m aan x
  inv
        ## Return a matrix that is the inverse of 'x'
}

cacheSolve(a)
a$getinv()  # this is only to show you that the mean has been stored and does not affect anything
cachemean(a)
a$set(c(10,20,30,40))
a$getinv()
cachemean(a)
cachemean(a)
a$get()
a$setmean(0)  # do NOT call setmean() directly despite it being accessible for the reason you will see next
a$getmean()
a$get()
cachemean(a)  # as you can see the call to setmean() effectively corrupted the functioning of the code
a <- makeVector(c(5, 25, 125, 625))
a$get()
cachemean(a)
cachemean(a)















#example
makeVector <- function(x = numeric()) {
  m <- NULL # mean moet nog berekend worden
  set <- function(y) { # om vector x evt te overschrijven met nieuwe data
    x <<- y # vector x wordt overschreven met vector y van buitenaf
    m <<- NULL # mean moet (opnieuw) berekend worden
  }
  get <- function() x # kun je aanroepen om vector x te kunnen zien
  setmean <- function(mean) m <<- mean # geef evt een waarde van mean m van buitenaf
  getmean <- function() m # laat zien wat mean m is
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


a <- makeVector(1:4)
a$get()
a$getmean()


cachemean <- function(x, ...) {
  m <- x$getmean() # bekijk of er een een waarde van m aanwezig en zo ja wat deze dan is
  if(!is.null(m)) { # indien er al een waarde is
    message("getting cached data") # geef dan deze tekst weer
    return(m) # en ook de waarde van m
  }
  data <- x$get() # bekijk de vector 
  m <- mean(data, ...) # bereken de mean van vector x
  x$setmean(m) #  # geef berekende waarde van m aan x
  m
}


cachemean(a)
a$getmean()  # this is only to show you that the mean has been stored and does not affect anything
cachemean(a)
a$set(c(10,20,30,40))
a$getmean()
cachemean(a)
cachemean(a)
a$get()
a$setmean(0)  # do NOT call setmean() directly despite it being accessible for the reason you will see next
a$getmean()
a$get()
cachemean(a)  # as you can see the call to setmean() effectively corrupted the functioning of the code
a <- makeVector(c(5, 25, 125, 625))
a$get()
cachemean(a)
cachemean(a)
