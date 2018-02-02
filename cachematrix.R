## This program inverts a matrix. sinceinversionof amatrix is
## computationally expensive, the programuse lexical scopingof R
## to cache the computed inverse of a matrix. if another inverseof 
## the samematrix is needed, ituses the cached value

## Thifunction takes a matrix and return eithe a computed
## a cached inverse of the matrix.

makeCacheMatrix <- function(m = matrix()) {
  im <- NULL
  set <- function(y){
    m <<- y
    im <<- NULL
  }
  get <- function() m
  setim <- function(solve) im <<- solve
  getim <- function() im
  list(set = set,get = get,
       setim = setim,
       getim = getim)
}


## This function conutes the iverse of a matrix if the matrixis
## being visited for the first time. If the mtrix has already in
## inthe cache, it return the cached value

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
  im <- m$getim()
  if(!is.null(im)){
    message("gettingcached data")
    return(im)
  }
  data <- m$get()
  im <- solve(data, ...)
  m$setim(im)
  im
}
