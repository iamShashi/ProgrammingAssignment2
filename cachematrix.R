##Below are two functions that are used to create a special object that stores a numeric matrix 
##and caches its inverse.

##The first function, makeCacheMatrix creates a special "matrix" object, which is really a list 
##containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the matrix inverse
##get the value of the matrix inverse

makeCacheMatrix <- function(x =  matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


##The following function calculates the inverse of the special "matrix" created with the above function. 
##However, it first checks to see if the inverse has already been calculated (and the matrix has not changed) 
##and if so, then it gets the inverse from the cache and skips the computation. Otherwise, it calculates 
##the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.
##This function assumes that the matrix supplied is invertible.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...) ## compute the inverse of the matrix
  x$setinverse(m)
  m
}

##Test code
##matrix(1:4,2,2)
##mymatrix <- makeCacheMatrix (matrix(1:4,2,2))
##cacheSolve(mymatrix)
