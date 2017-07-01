## cacheSolve retrieves a special matrix object from the user defined function makeCacheMatrix and then uses it to calculate the inverse matrix. 
## If the inverse matrix has been calculated, then cacheSolve retrieves the inverse from the cache.


## This function creates a special "matrix" object that can cache its inverse

## Initialize two objects, x and m.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## If inverse has been solved and stored in m, set to clear for when x is reset
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Utilize lexical scoping rules by defining x outside of get() and retrieve x from parent environment
  get <- function() x
  ## Define "getters and setters" for the inverse
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  ## Name each element in the list to allow use of the '$' form of the extract operator in downstream code
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix above or populates the inverse from the cache. 

cacheSolve <- function(x, ...) {
  ## Attempt to retrieve an inverse by calling the getinverse() function on the input object
  m <- x$getinverse()
  ## Check to see whether the result is NULL. If the value is not equal to NULL, there exists a valid, cached inverse.
  if(!is.null(m)) {
    message("getting cached data")
    ## Return existing inverse matrix to the parent environment
    return(m)
  }
  ## If the result of !is.null(m) is FALSE, then cacheSolve() gets the matrix from makeCacheMatrix and calculates an inverse by utilizing solve().
  data <- x$get()
  m <- solve(data, ...)
  ## Apply setinverse() function to the input matrix to set the inverse in the input object.
  x$setinverse(m)
  ## Print the inverse matrix to the parent environment
  m
}
