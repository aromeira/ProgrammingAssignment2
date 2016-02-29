# Programming Assignment 2: Lexical Scoping
#
# This second programming assignment will require you to write an R function 
# that is able to cache potentially time-consuming computations. For example, 
# taking the mean of a numeric vector is typically a fast operation. However, 
# for a very long vector, it may take too long to compute the mean, especially 
# if it has to be computed repeatedly (e.g. in a loop). If the contents of a 
# vector are not changing, it may make sense to cache the value of the mean so 
# that when we need it again, it can be looked up in the cache rather than 
# recomputed. In this Programming Assignment you will take advantage of the 
# scoping rules of the R language and how they can be manipulated to preserve 
# state inside of an R object.
# ----------------------------------------------------------------------------
# CacheMatrix:
# This function below creates a list that contains the functions: set, get
# setInv and getInv. it uses <<- assignment operator so that
# these internal variables are not exposed to the outside environment. 
#
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL # this is the variable of inversion results is stored
 
    set <- function(y) {  # set function but not the inverse
    x <<- y
    i <<- NULL   
  }
  get <- function() x   # get function 
  setInv <- function(inv) i <<- inv # Manually set the inverse
  getInv <- function() i # Get the inverse
  list(set = set, get = get,  # Encapsulate into a list
       setInv = setInv,
       getInv = getInv)
}
# CacheSolve:
# After creating the matrix, CacheSolve function calculates the inverse of the 
# matrix and stores the results in cache. If you try to use the function again 
# in the same array, the results shown are the values that have been calculated, 
# avoiding unnecessary processing.
#
cacheSolve <- function(x, ...) {
  m <- x$getInv() # get the inversed matrix from object x
  # it will be null if uncalculated, remember the first line (i <- NULL) in the previous function
  if(!is.null(m)) { # if the inversion result is there
    message("getting cached data")
    return(m) # return the calculated inversion
  }
  data <- x$get() # if not, we do x$get to get the matrix object
  m <- solve(data) # we solve it
  x$setInv(m) # we then set it to the object
  m # return the solved result
}
# 
# 
# 
# 
# Test
# generate a random square, non-singular matrix
test <- matrix(runif(4,1,100),2,2)
# generate the makeCacheMatrix object with this matrix
testIsCached <- makeCacheMatrix(test)
# from now on calculate or retrieve calculated inversion using the cacheSolve function

test2 <- cacheSolve(testIsCached)
print(test2)
test2 <- cacheSolve(testIsCached)
print(test2)
test2 <- cacheSolve(testIsCached)
print(test2)
