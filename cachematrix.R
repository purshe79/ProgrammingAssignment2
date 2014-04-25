## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function stores a matrix & its inverse in the cache(persistent between function calls).
## The makeCacheMatrix also provides read(get) and write(set) methods for accessing the two stored matrices
## The inverse of the matrix is actually calculated outside the makeCacheMatrix in the cacheSolve function
## The cacheSolve creates the inverse of a matrix only during the first invokation & thereafter avoids fresh calculation by simply accessing the cached inverse
## The cacheSolve assumes the input is a non-singular square matrix

## Write a short comment describing this function

## The makeCacheMatrix function creates a matrix object with 4 methods attached to the object.
## 1. get() - returns the value of the matrix
## 2. set() - initializes or modifies the value of the matrix
## 3. getinverse() - returns the inverse of the matrix object
## 4. setinverse() - stores the inverse of the matrix object(which is calculated outside and passed to this method)

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(x) inverse <<- x
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve is a function that calculates the inverse of a matrix
## cacheSolve takes as input a matrix object (created by makeCacheMatrix() function)
## cacheSolve returns as output the inverse of the input matrix
## If the cacheSolve is being invoked for the first time, it calculates the inverse and stores it in a cached variable
## Every time cacheSolve is invoked after the first time, it just reads the already calculate value from the cached variable

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
