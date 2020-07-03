## The function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {# define the argument with default mode of "matrix"
  inv <- NULL #initialize inv as NULL; will hold value of "matrix inverse
  set <- function(y){ # define set function to assign new
    x <<- y             # value of matrix in parent environment
    inv <<- NULL             #if there is a new matrix, reset inv to NULL
  }                       
  get <- function() {x}       ## define the get function - returns value of matrix argument
  setInverse <- function(inverse) {inv <<- inverse} # assigns value of inv in where called
  getInverse <- function() {inv}                    # gets the value of inv where called
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse) # refer to the functions with $ operator
}
cacheSolve <- function(x, ...) { # This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data<- x$get()
  inv<- solve(data, ...)
  x$setInverse(inv)
  inv
}
