## This section helps calculate the inverse of the inputted matrix

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function(y){
    x<<-y 
    m<<-NULL ##defines a function to set the matrix, x, to a new matrix, y, and resets the inverse, m, to NULL
  }
  get <- function() x                            ##returns the matrix x
  setinverse <- function(solve) m<<- solve      ##sets the inverse matrix to m
  getinverse <- function() m                     ## returns the inverse m
  list(set= set, get= get, setinverse=setinverse, getinverse=getinverse)
}



#### This section gets the previously calculated inverse if the matrix is same as the previous one, otherwise calculates the inverse again

cacheSolve <- function(x= matrix(), ...) {    ## this compares the matrix to the previously set matrix
  m <- x$getinverse()                          ## this step gets the inverse if the latter has been calculated
  if(!is.null(m)){
    message("getting cached data")
    return(m)                                 ## this step checks if m is null, if it is not, this gets the cached value of m already calculated
  }
  matrix<-x$get()                             ## run the get function to get the value of the input matrix
  m<-solve(matrix, ...)                      ## compute the value of the inverse of the input matrix
  x$setinverse(m)                           ## this step runs the setinverse function on the inverse to cache it
  m                                        ##returns the inverse
}
