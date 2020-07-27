## Put comments here that give an overall description of what your
## functions do


## write a pair of functions that cache the inverse of a matrix.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get <- function() x
  setinv <- function(inverse) {inv <<- inverse}
  getinv <- function() {inv}
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    reture(inv)
  }
  mat <- x$get()
  inv <-solve(mat,...)
  x$setinv(inv)
  inv
}

## self-testing
my_matrix <- makeCacheMatrix(matrix(1:4,2,2))
my_matrix$get()
my_matrix$getinv()
cacheSolve(my_matrix)