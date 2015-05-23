##The first function, "makeCacheMatrix", creates a special "vector", which
##is really a list containing a function to

##1. set the value of the matrix.
##2. get the value of the matrix.
##3. set the value of the inverse of the matrix.
##4. get the value of the inverse of the matrix.
makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(inv) inv<<-solve(x)
  getinv<-function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
##This 2nd fuction calculates the inverse of the matrix,  If the inverse 
##has already been calculated (and the matrix has not changed), then 
##the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinv(inv)
  inv
}##Return a matrix that is the inverse of 'x'

