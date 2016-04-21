##   A pair of functions to cache the inverse of a matrix, so
##  when an inverse of a matrix has been solved once, there is no
##  need to make the calculations again thus saving lots of 
##  computation time


## makeCacheMatrix generates a list of functions assigned to
## a given "x" matrix with to:
## 1.- set: initialize it, attaching it "inver" variable 
## 2.- get: retrieve "x" matrix values
## 3.- setinver: assign a value to its internal "inver" variable
## 4.- getinver: retrieve "inver" value


makeCacheMatrix <- function(x = matrix()) {
      inver <- NULL
      set <- function(y) {
            x <<- y
            inver <<- NULL
      }
      get <- function() x
      setinver <- function(solve) inver <<- solve
      getinver <- function() inver
      list(set = set, get = get,
           setinver = setinver,
           getinver = getinver)
}


## Making use of makeCacheMatrix, returns inverse of matrix "x"
## Firs verify if matrix "x" had been previously solved, otherwise
## solves it

cacheSolve <- function(x, ...) {
  
        # First attempt
        inverseMat <- x$getinver()
  
        # Was the inverse calculated previously? If so, return it
        if (!is.null(inverseMat)) {
          message("getting cached data")
          return(inverseMat)
        }
        # inverseMat is null, so it need to be computed
        message("solving...")
        data <- x$get()
        inverseMat <- solve(data)
        # store it in order to avoid computing it again
        x$setinver(inverseMat)
        #... and finally return inverseMat
        inverseMat
}
