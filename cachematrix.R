## These 2 functions use the lexical scoping properties of R to impliment the 
## cacheing of an function that may be to calculation intensive to execute
## everytime you need the result. In this case we are cacheing the inverse matrix
## function as an example
##
## The overall strategy of the 2 functions is to passback a list from the function 
## makeCacheMatrix giving access to the entire function environment from the parent
## environment. Access to the full MakeCacheMatrix environment allows the object
## to access the internal structure of the function including nested functions 
## set,get,setinverse,getinverse)

## The makeCacheMatrix function returns a list of the nested functions to the calling
## object. Essentially implimenting an "object oriented" structure in the R working space.
##
## The 4 functions implimented are:
##	set() - initialize the input matrix to a new value and set the inverted matrix
##	        value to null (for managing the calculate/cache decisin in cacheSolve)
##	get() - returns the current input matrix in the "object"
##	setinverse - updates the inverse matrix if recalculated by cacheSolve
##	getinverse - returns the current inversed matrix, if the matrix has not yet been
##                 calculated a NULL is returned


makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
     	set <- function(y) {
                x <<- y
                i <<- NULL
     	}
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The cacheSolve function is used to either retrieve a previously calculated inverted matrix
## or if the currently cached inverse matrix is NULL, it calculates the inverse matrixe of the
## current contents of the input matrix using the R solve() function and stores it back in the 
## cache for future  reference.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

 	i <- x$getinverse()
      if(!is.null(i)) {
      	message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
