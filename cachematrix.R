## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix expect a square matrix as input; if nothing is provided, an 
# empty matrix is created; there are four functions belong to this function 
# which are meant to manipulate the matrix and give inverse
# $set and $ get for set and return the matrix, 
# $setInv and $getInv for set and return the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
# cacheSolve expect resulting object of makeCacheMatrix as input; 
# should inverse of the matrix (m) are not null, cacheSolve will simply 
# return msg 'getting cached data' and then the existing inverse matrix without
# fruther computed again. If not use $setInv which is an element of makeCacheMatrix
# to compute and return inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}

#tmp$get();#tmp$set(diag(3));#tmp$getInv();#cacheSolve(tmp);#tmp$setInv(diag(4))
