## A group of functions to create a cache for inverse of a matrix


## makeCacheMatrix creates a "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) { 
  m <- matrix(data=NA) #initializes the matrix to a single NA (can't use NULL here) 
  set <- function(y) { 
    x <<- y #stores the matrix to be inverted 
    m <<- NA #flushes m to instruct cachesolve that there is no inverted one 
  } 

  get <- function() x #returns the matrix to be inverted 

  setinverted <- function(inverted) m <<- inverted #assigns the inverted matrix to m (caching) 
  getinverted <- function() m #returns the inverted matrix stored in m (cached) 

  list(set = set, get = get, 
       setinverted = setinverted, 
       getinverted = getinverted) #creates the list of functions 

} 

cachesolve <- function(x=matrix(), ...) { 
  m<-matrix(data=NA)
  #initializes the matrix to a single NA 
  
  m <- x$getinverted()
  #gets the inverted matrix and assigns it to local m 
  
  if(all(!is.na(m))) { 
  #if result is not an NA matrix then the value has been cached 
    message("getting cached data") #and it shows it's the case... 
    return(m) #returns m and exits 
  } 

  #if the above condition is not met then it's time to create the invertse and store it 
  data <- x$get() #gets the matrix to be inverted 
  m <- solve(data, ...) #inverts it 
  x$setinverted(m) #passes it to the special matrix object 
  m #returns the local copy 
} 
 




