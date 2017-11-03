

#Cache inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
     i <- NULL
            s <- function(y) {
                      x <<- y
                      i <<- NULL
              }
          g <- function() x
          si <- function(solveMatrix) i <<- solveMatrix
          gi <- function() i
          list(set = s, get = g,
                              setinverse = si,
                               getinverse = gi)
}


#returns the inverse of the matrix from makeCacheMatrix above

  
  cacheSolve <- function(x, ...) {
   
             i <- x$getinverse()
           if(!is.null(i)) {
                     message("don't worry, data is in cache")
                     return(i)
             }
           d <- x$get()
           i <- solve(d)
           x$setinverse(i)
           i
  }
  


  