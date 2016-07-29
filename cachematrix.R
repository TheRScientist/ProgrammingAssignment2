## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## MakeCacheMatrix Function stores matrix in memory

makeCacheMatrix <- function(X = matrix()) {
        inverse <- NULL
        set <- function(Y){
                X <<- Y
                inverse <<- NULL
        }
        get <- function() X
        setinverse <- function(Inverse) inverse <<- Inverse
        getinverse <- function() inverse
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
##cacheSolve first checks to see if the matrix has already been calculated. If not it calculates the inverse of the matrix.

cacheSolve <- function(X, ...) 
{
        if(!is.null(inverse)){
                message("matrix is in memory")
                return(inverse)
        }
        message("inverse is not in memory so the inverse (if exist) is gonna be computed")
        data <- X$get()
        inverse <- pseudoinverse(data, ...)
        X$setinverse(inverse)
        inverse
}

