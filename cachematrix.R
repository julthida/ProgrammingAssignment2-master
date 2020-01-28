## Caching the Inverse of a Matrix
## Below are two functions that are used to create a
## special matrix and caches its inverse

## creates the list for cachesolve

makeCacheMatrix <-function(x=Matrix()){
  inverse<<-NULL
  
  setmatrix<-function(y){
    x<<-y
    inverse<<-NULL
  }
  
  getmatrix<-function() x
  
  setinverse<-function(x) {
    inverse2<-matrix(nrow=ncol(x),ncol=nrow(x))
    for(i in seq(1,nrow(x),by=1)){
      inverse2[i,]<-x[,i]
    }
    inverse<<-inverse2
  } 
  
  getinverse<-function(){
    return(inverse)
  }
  
  cache<<-(list(set=setmatrix,get=getmatrix,setinv=setinverse,getinv=getinverse))
}

## computes the inverse of the special "matrix" returned by `makeCacheMatrix` 
## retrieve the inverse from the cache if the inverse already been calculated

cacheSolve <- function(x) {
  inverse<-cache$getinv()
  
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  data<-cache$get()
  inverse<-cache$setinv(data)
  cache$setinv(inverse)
  return(inverse)
}