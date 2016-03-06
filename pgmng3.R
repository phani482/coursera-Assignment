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

test <- function(mat){
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheinverse(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheinverse(temp)
  dur = Sys.time() - start.time
  print(dur)
}
## calling functions
set.seed(1110201)
r = rnorm(1000000)
mat1 = matrix(r, nrow=1000, ncol=1000)
test(mat1)

