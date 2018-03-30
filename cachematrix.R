## Functions to cache and inverse matrix
##makecacheMatrix function gets and set the matrix
##cachSolve function for the matrix inverse

#Function to cache Matrix Inverse 
makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  #set the matrix
  set <- function(y)
  { x <<- y
  minv <<- NULL
  
  }
  #Get the matrix to be inversed
  get <- function() x
  #Set the inversed matrix
  setinv <- function (solve) x <<- solve
  #Return the inversed matrix
  getinv <- function() minv
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}

#Function for the Matrix inverse
#Parameter passed to be of matrix type
cacheSolve <- function(x, ...) {
  #check matrix is in cache
  minv <- x$getinv()
  if(!is.null(minv)) {
    print("getting cached data")
    return(minv)
  }
  #if not in cache, find the inverse set it 
  data <- x$get()
  minv <- solve(data, ...)
  x$setinv(minv)
  minv
}
