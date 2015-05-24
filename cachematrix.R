

#Function for caching the inverse of a matrix

makeCacheMatrix <-function(M=matrix()) { #In the argument we take a matrix which is empty
  
  
  #We create an object named "matrix" for caching the inverse matrix
  matrix=matrix()
                                           
   #This function sets the matrix N to M
   set<-function(N){
                     M<<-N
                     matrix<<-matrix()
                   }
  #Returns the matrix M
   get<-function(){ M }
  #Sets the inverse of the matrix I to the object named "matrix"   
  setInverse<-function(I){matrix<<-I  }
  
  #Returns the inverse of the matrix 
  
  getInverse<-function(){ matrix}
  
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)

}


#Assigning the inverse from the cache

cacheSolve <- function(K, ...) {
        ## Return a matrix that is the inverse of 'K'
  matrix <- K$getinverse()
  
  #Checks whether the matrix exists or not
  if(!is.null(K)) {
    
    #Checks whether the matrix 'K is equal to 'M' or not
    if(K==M){
      message("getting cached data")
      return(matrix)      
      
    }
  }
  
  data <- x$get()
#Calculating inverse of a matrix by solve() function
  matrix <- solve(data, ...)
#Invoking setInverse function and setting the inverse of matrix 'K' as "matrix"
  K$setinverse(matrix)

  matrix
}
