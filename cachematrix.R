##Please, for doubts about using this functions, refeer to the thread
##https://class.coursera.org/rprog-005/forum/thread?thread_id=482
##-----------------------------------------------------------------------##

#How to use:
#create a initial matrix: a<-matrix(1:4,2,2)

#call a2<-makeCacheMatrix(a)

#using a2$get(), see that the matrix 'a' is now stored

#a2$getinverse(), see that the inverse of 'a' is either cached or NULL

#if the result of a2$getinverse() was NULL, call cacheSolve(a2)

#call again a2$getinverse() and see that now the value is cached

#call again cacheSolve(a2) and see that it didn't compute the inverse matrix again
#instead, it displayed the result that was cached (it called a2$getinverse() for you)

#Use a2$set(Matrix) if you want to change the initial matrix for another one

#start the whole process again.

##-----------------------------------------------------------------------##
##The makeCacheMatrix function caches the inverse of a matrix 'X'.
##The input matrix should ALWAYS be a squared matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #sub-function 1: stores the original matrix
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    #sub-function 2: displays the original matrix
    get <- function() x
    
    #sub-function 3: stores the inverse matrix
    #this sub-function should never be called from any other place
    #other than the function cachesolve
    setinverse <- function(solve) m <<- solve
    
    #sub-function 4: displays the inverse matrix
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  
##-----------------------------------------------------------------------##
##The cacheSolve function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
      ## get cached inverse matrix from MakeCacheMatrix function
      m <- x$getinverse()
      
      ##If the returned value is not NULL, it means that it was cached already
      ##So just return the value and finish the function
      if(!is.null(m)) {
        message("getting cached inverse matrix!")
        return(m)
      }
      
      ##Else, get the original matrix, calculate it's inverse
      ##Cache the result into the function MakeCacheMatrix
      ##and finally print the result.
      message("First time doing this calculation. The result will
              be cached for future consultations.")
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
    }
##-----------------------------------------------------------------------##