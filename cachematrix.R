## Put comments here that give an overall description of what your
## functions do

## This code is composed by 2 functions that allow us set , store in cache and get the value of a matrix and it inverse.
## In fact this cache(a reserved piece of memory), is stores in an environment diferrent from the current environment, 
## and is only available through the function stored in makeCacheMatrix (we cant'a accesss this cache outside this function)
## The matrix is stored in cache througth the global assignment operator  (<<-).It is also possible through the function Assign.
##
## This method is particulary important to save the result of expensive computations,
##  avoiding the recomputation if nothing changed since the last computation.
##
## References:
##  http://adv-r.had.co.nz/Functional-programming.html
##    See section Mutable State
##  http://stackoverflow.com/questions/2628621/how-do-you-use-scoping-assignment-in-r
##
##
##
##
##
##
##
#Example of usage:
####1 - create a new object makeCacheMAtrix, where x matrix is null.This instruction resets the object  
####1 - , and the cache (x and inv) even if the x matrix is equal to the previous one
##      a <- makeCacheMatrix()  
####2 - create a new mattrix
##      newmat<-rbind(c(1, -1/4), c(-1/4, 1)) 

####3 - Set te value of the x matrix to newmat
##      a$set(newmat) 
#### Note: Step 2 and 3 can be at the same rtime ina signle instruction: a$set(rbind(c(1, -1/4), c(-1/4, 1)) ) 
#### Note: We can create a new makeCacheMAtrix object and set the value of the x matrix in one single step 
##    a <- makeCacheMatrix(newmat)
####4 - get the x matrix from cache , if already exists
##      a$get() 
####5 get the inverse of the original matrix, (if)stored  in cache (if), according to the rules 
####5 mentioned in the resolvecache comments 
##      cacheSolve(a)
##
##
##
##
##
#### Reset both functions definition and the object (could be useful in the development phase )
##      if (exists("makeCacheMatrix")) rm(makeCacheMatrix)
##      if (exists("cacheSolve"))rm(cacheSolve)
##      if (exists("a"))rm(a)




##----Function makeCacheMatrix----
## The functions makeCacheMatrix, receives a parameter x, which is a matrix, and is composed by  a list of 4 diferent functions:
##      set    : stores a matrix in cache
##      get    : get a matrix already stored in cache
##      setinv : Stores a 2nd mmatrix, the inverse of the 1st one, in cache
##      getinv : get the inverse matrix  from cache if it's already stored in cac

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
  set <- function(y) {
    x <<- y
    inv<<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse){ 
  inv <<- inverse 
  }
  
  getinv <- function() inv
  
  list(
       set = set
      ,get = get
      ,setinv = setinv
      ,getinv = getinv
       )

}



##-----Function resolvecache----
##  The function resolvecache, receives a parameter of type makeCacheMatrix (x) and do the following:
##  1 - If the the inverse matrix exists in x's cache (the object returned by x$getinv() is not null )  then retrieves the inverse matrix stored in x withou any computation
##      "getting cached data" follwing by the invsere matrix
##  2 - if the inverse matrix doesn't exists in x's cache( object returned by x$getinv() is  null ) the inverse matrix is computed and sored in the x's cache .
##  3 - If the x dossn't exist an error is thrown
##  4 - if the x doesn't contain any original matrix, a matrix with NA is returned
##  5 - if the original matrix is not square and error is thrown
##  6 - If the original matrix isn't invertible an errow is thrown   



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    inv_cached<- x$getinv()
  if(!is.null(inv_cached) 

     ) {
    message("getting cached data")
    return(inv_cached)
  }
 
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)

  inv

  
}




