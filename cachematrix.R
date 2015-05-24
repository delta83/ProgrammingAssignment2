## This file aims to cache the inverse of an invertible matrix so that it is not solved each time.
## It contains the functions makeCacheMatrix and cacheSolve.
## makeCacheMatrix function defines the four functions namely set, get, setinverse and getinverse and returns a list of that.
## cacheSolve function get the inverse of the matrix from cache if already present otherwise computes the inverse and sets it in the cache.

##makeCacheMatrix function defines the four functions namely set, get, setinverse and getinverse and returns a list of that.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL ## inverse variable will store the inverse of the matrix, Here it is initialized as NULL
  set <- function(y) { ## set function sets the matrix for which the inverse needs to be calculated 
    x <<- y
    inverse <<- NULL ## initializes the inverse for the matrix as NULL
  }
  get <- function() x ## get function returns the matrix for which the inverse is calculated
  setinverse <- function(inv) inverse <<- inv # setinverse function sets the inverse in the cache
  getinverse <- function(y) return(inverse) ## getinverse function gets the inverse from the cache and returns it
  return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}

## cacheSolve function get the inverse of the matrix from cache if already present otherwise computes the inverse and sets it in the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse() ## gets the inverse from the cache for the matrix
  if(!is.null(i)) { # if the inverse retrieved is not null then return this inverse
    print("Getting the inverse from cache.")
    return(i)
  } else { ## if the inverse retrieved in NULL, then the inverse is not set in cache
    data <- x$get() ## get the matrix
    x$set(data) ## set this matrix in cache
    i <- solve(data) ## compute the inverse of this matrix
    x$setinverse(i) ## set this inverse in the cache fro later retrieval if needed
    print("Solving the inverse.")
    return(i) ## return the computed inverse matrix
  }
}
