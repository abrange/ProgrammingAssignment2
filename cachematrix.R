## This file contains 2 functions which allow create an special "Matrix" which supports store their inverted value
## The second one allows calculate the inverted of a special Matrix but using the cached value when it is already
## saved (cached)
## Author: Alvaro Brange

## This function returns a list object which support to store a inverted matrix as cache. 2 objects are saved
## The matrix and the inverted matrix. This support to be initilized on first call of after using $set
## If original matrix is modified ($set) the inverted matrix will be cleaned to force to recalculate it
makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set <- function(y){
        #each time the original matrix is updated, the cache is cleaned, so inverted matrix is removed
        x<<-y
        i<<-NULL
    }
    get <- function() x
    setinverse <- function(inve) i<<- inve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

## cacheSolve returns the inverse of matrix x (given as parameter)
## but tries get a inverse matrix from cache of x before calculate it
## if there is not a cached inverted matrix (x$getinverse) the inverted will be calculated and it will be set 
## into cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    li <-x$getinverse()
    #check if already is a inverted matrix saved
    if(!is.null(li)) {
        #already exists a inverted matrix, so it is returned avoiding calculate inverse again
        message("getting cached inverted matrix")
        return(li)
    }
    #there is no a inverted matrix saved, so it is calculated and stored into the special matrix object
    data <- x$get()
    li <- solve(data,...) #inverted matrix is calculated
    x$setinverse(li) #inverted matrix is saved
    li
}
