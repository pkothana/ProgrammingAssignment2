## This R Program caches the inverse of a matrix, which potentially involves time consuming computations 
## It has 2 functions makeCacheMatrix (creating matrix object and caching inverse) and 
## cacheSolve (calculates inverse of matrix - first checks cache)


## The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse
## It requires numeric matrix as its only argument

makeCacheMatrix <- function(x = matrix()) {

  cachedmx <- NULL                    ## Assigning NULL to local variable cachedmx (initialising cache as NULL)                              
  
  ## set function to reset cached inverse matrix cachedmx as NULL
  set <- function(yx) { 
    x <<- yx                          ## assigning passed value to numeric matrix 
    cachedmx <<- NULL                 ## setting the cached matrix as NULL
  }

  get <- function() x                 ## function to return the formal variable (input matrix) x
  
  setinverse <- function(InvMat) cachedmx <<- InvMat 
                                      ## Assigning new matrix to cache variable cachedmx

  getinverse <- function() cachedmx   ## function to return the cache variable cachedmx
    
  list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)  
                                      ## returning the list of functions as final output of function makeCacheMatrix 
}


## The function "cacheSolve" computes the inverse of a matrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  mx <- x$getinverse()                ## calling getinverse from makeCacheMatrix and getting existing cached value to mx
  
  ## if matrix cache is existing, return cached matrix
  if(!is.null(mx)) {           
    message("getting cached matrix")  ## show the message that result is returned from cached matrix
    return(mx)                        ## return cached inverse matrix
  }
  
  ## If not cached, follow this steps to calculate inverse and set in cache
  mat <- x$get()                      ## assigning the input matrix to variable mat
  inversematrix <- solve(mat)         ## calculating inverse of matrix and assigning to local variable mx 
  x$setinverse(inversematrix)         ## setting the inversed matrix to cache 
  inversematrix                       ## Returning the new computed inverse of input matrix x
}
