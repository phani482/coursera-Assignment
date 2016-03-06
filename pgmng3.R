##Set the value of the matrix
##get the value of the matrix
##set the value of the inverse of a matrix
##get the value of the matrix
##

makeCacheMatrix <- function(x=matrix()){
  inv<-NULL
  ##Set the value of the matrix 
  set<-function(y){
    x<<- y
    inv<<-NULL
  }
  ##get the value of the matrix
  get <- function(){
    x
  }
  setinverse<-function(inverse){
  inv<<-inverse  
  } 
  getinverse<-function(){inv}
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.

cacheinverse<- function(x, ...) {
  inv<- x$getinverse()
  ## if the inverse has already been calculated
  if(!is.null(inv)) {
    ## get it from the cache and skips the computation.
    message("getting cached data")
    return(inv)
  }
  #otherwise, calculates the inverse 
  mat.data =x$get()
  inv=solve(mat.data, ...)
  # sets the value of the inverse in the cache via the setinv  function
  x$setinverse(inv)
  return(inv)
}

## Inverse testing

##test <- function(mat){
##  temp = makeCacheMatrix(mat)
##  
##  start.time = Sys.time()
##  cacheinverse(temp)
##  diff = Sys.time() - start.time
##  print(diff)
  
##  start.time = Sys.time()
##  cacheinverse(temp)
##  diff = Sys.time() - start.time
##  print(diff)
##}
## calling functions
##set.seed(110202)
##r = rnorm(10000)
##mat1 = matrix(r, nrow=100, ncol=100)
##test(mat1)

