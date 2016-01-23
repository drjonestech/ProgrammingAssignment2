## Cristal Jones, Programmer
## The makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.
## cacheSolve is within makeCacheMatrix functions - Lexical Scoping

## set and get matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y){
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse)inv <<- inverse
    getInv <- function() inv
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
   
    
   

}
## Inverse function; works with 2 X 2 matrix
## The cacheSolve function will compute the inverse of makeCacheMatrix only if the inverse has not been calculated.
cacheSolve <- function(x, ...) 
  {
  ## Return a matrix that is the inverse of 'x'
    inv <-x$getInv()
    if(!is.null(inv)){
      message("getting cached data")
      return (inv)
      
    }
    m <- x$get()
    inv <- solve(m,...)
    x$setInv(inv)
    inv
}
        
   
   
  
 
    
   
  

