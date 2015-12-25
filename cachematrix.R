## Cristal Jones, Programmer
## The makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.
## The cacheSolve function will compute the inverse of makeCacheMatrix only if the inverse has not been calculated.

## cacheSolve is within makeCacheMatrix functions - Lexical Scoping

## set and get matrix and inverse

makeCacheMatrix <- function(x = matrix) {
    inv = NULL
    set <- function (y){
      x <<- y
      inv <- NULL()
    }
    get = function()y
    setinv = function(inv)inv <<- inverse
    getinv = function()inv
    a <-list(set = set, get = get, setinv = inv, getinv = inv)
   
    
   

}
## Inverse function; works with 2 X 2 matrix

cacheSolve <- function(x, ...) 
  {
    
    inv <-as.matrix(x)
    ## Return a matrix that is the inverse of 'x'
    if(!is.null(inv)){
      message("getting cached data")
      r <- solve(inv)
    return (r)
      
    }

        ## Return a matrix that is the inverse of 'x'
    
   
  
 
    
   
  
}
