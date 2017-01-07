## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix, creates a set of R functions that are contained in a list:
#set is a function to set the value of the matrix but is not used in the second function.
#get is a function that retrieves the value of the matrix from x.
#setinverse is a function that sets the value of the inverse matrix
#getinverse is a function that returns the value of the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)    
}

## The function below, cacheSolve, calculates the inverse of the matrix through use of the functions built in the makeCacheMatrix function.
## It checks if the inverse has been calculated already.  If it has, it returns the cached inverse.  
## If the inverse has not been not calculated already, it calculates the inverse.

cacheSolve<-function(x, ...) { 
     #Checks to see if the inverse has already been calculated     
     m<-x$getinverse() 
     if(!is.null(m)) { 
          message("retrieving cached data") 
          return(m) 
     } 
     #Calculates the inverse of the matrix
     data<-x$get() 
     m<-solve(data, ...) 
     x$setinverse(m) 
     m 
} 

#Example
#Creates a matrix
InitMatrix <- matrix(c(2, 4, 4, 2), 2, 2)    
InitMatrix
#Creates the inverse of the matrix
InvMatrix <- makeCacheMatrix(InitMatrix) 
cacheSolve(InvMatrix)
#Demonstrates the cache function works
cacheSolve(InvMatrix)
