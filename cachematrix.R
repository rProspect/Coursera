## This function can be used to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
## 1. Set the value of the vector
## 2. get the value of the vector
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) { ## initialise x as part of makeCacheMatrix(x) 
  s <- NULL ## initialise i as variable where the inverse will be stored
  set <- function(y) { ## assigns data values to an object in parent env.
    x<<-y ## assign input argument to the x object in the parent env.
    s <<-NULL ## assign a NULL value to the i object in the parent env.
  }
  get<-function() x ## defines the getter for x, retrieve x from parent env of makeCacheMatrix
  setsolve<-function(solve) s<<-solve ## defines the setter for inverse i, assign input argument to the value of i
  getsolve<-function() s ## defines the getter for the inverse i
  ## assigns the 4 functions as elements within a list and return it to 
  ## parent environment
  ## each element is named to allow for the use of $ extract operator
  list(set=set, get=get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## This function computes the inverse of a special "matrix" returned by the 
## makeCacheMatrix
## Firstly, check to see if the inverse of the matrix was already calculated
## if so, get the inverse form the cache and skip the computation
## otherwise, calculate the inverse of the matrix
## and set value of the inverse in the cache via the setsolve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
  
}
