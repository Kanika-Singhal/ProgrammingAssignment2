##The two functions contained in this file calculate the inverse of a matrix and cache it.
##When the inverse is requested, the functions check if an inverse is already cached.
##If yes, they return the cached inverse
##If not, they calculate the inverse and return



## makeCacheMatrix() takes a matrix x as an input, and returns a list of 4 functions based on x
#So if you enter 

#  >y <- matrix(1:4, nrow = 2, ncol = 2)
#  >xyz <- makeCacheMatrix(y)

## then xyz will be a list of 4 functions
#1) 'set' - The value of 'x' is the matrix passed initially, but this function can change that to another matrix
#2) 'get' - Will return 'x'
#3) 'setInverse' - Will set the value of 'Inv' as the matrix passed to it
#4) 'getInverse' - Will return 'Inv'


makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inver) Inv <<- inver
  
  getInverse <- function() Inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}


## If you pass 'xyz' to cacheSolve(), it will first check if an inverse of 'y' is already cached.  
## If it is, it will return that inverse and exit
## If not, it will pull the matrix into 'data', calculate the inverse of 'data'
## then use setInverse() to cache the inverse matrix
## then return that inverse

#  >cacheSolve(xyz) 
## gives the inverse matrix of 'y' 
## To check, you can type 
#  > y %*% cacheSolve(xyz)
## which should give you an Identity matrix (top-left-to-bottom-right diagonal is 1, rest 0)

cacheSolve <- function(xyz, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- xyz$getInverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  
  data <- xyz$get()
  Inv <- solve(data, ...)
  xyz$setInverse(Inv)
  Inv 
}
