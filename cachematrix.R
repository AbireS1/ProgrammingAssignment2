## Put comments here that give an overall description of what your
## functions do
##the first function creates a special matrix x, this matrix is a list that has a function that is assigned to set
##the elements of the matrix and to get these elements. then it sets the elements of the inverse of the matrix in m and gets them. 
##the second function checks to see if the inverse of the matrix has already been calculated, if it was calculated, 
## it gets the inverse from the cache created with the first function, if not, it calculates the inverse of the matrix and 
##sets it in the cache usind the setinverse function.


## Write a short comment describing this function
##this function creates a special matrix x, this matrix is a list that has a function that is assigned to set
##the elements of the matrix and to get these elements. then it sets the elements of the inverse of the matrix in m and gets them. 

makeCacheMatrix <- function(x = matrix()) {
    m<- NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inverse) {m<<- inverse}
    getInverse <- function() {m} 
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
##the second function checks to see if the inverse of the matrix has already been calculated, if it was calculated, 
## it gets the inverse from the cache created with the first function, if not, it calculates the inverse of the matrix and 
##sets it in the cache usind the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    mat <- x$get()
    m <- solve(mat,...)
    x$setInverse(m)
    m
}



