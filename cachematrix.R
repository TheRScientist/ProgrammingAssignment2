## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## MakeCacheMatrix Function stores matrix in memory

makeCacheMatrix <- function(X = matrix()) {
        inv <- NULL
        set <- function(Y){
                X <<- Y
                inv <<- NULL
        }
        get <- function() X
        setinverse <- function(Inverse) inv <<- Inverse
        getinverse <- function() inv
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Write a short comment describing this function
##cacheSolve first checks to see if the matrix has already been calculated. If not it calculates the inverse of the matrix.

cacheSolve <- function(X, ...) 
{
        
        inv <- X$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        message("inverse is gonna be computed")
        data <- X$get()
        inv <- pseudoinverse(data, ...)
        X$setinverse(inv)
        inv
}
