##This function returns a list of functions 
##that can set the value and get the value of 
##the input matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
 I <- NULL
        set <- function(y){
                x <<- y
                I <<-NULL
        }
        get <- function()x
        setinverse <- function(inverse) I <<- inverse
        getinverse <- function() I
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


###this function gets the inverse of the matrix if it is cached
##otherwise it calculates the inverse using "solve" function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getinverse()
        if(!is.null(I)){
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data,...)
        x$setinverse(I)
        I
}


