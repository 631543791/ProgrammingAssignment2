## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix function creates a object to cache the inverse of inputed matrix
makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() solve
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


#this function computes the inverse of the matrix returned by makeCacheMatrix function above
cacheSolve <- function(x, ...){
        #return a matrix that is the inverse of x
        m <- x$getsolve()
        if(!is.null(m)){
                message("getting chached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setsolve(m)
        m
}
