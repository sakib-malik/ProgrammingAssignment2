## make cache matrix creates a special type of matrix 
## that have various functions like set , get, setInv, getInv
## to cache , get and update its inverse 
## cacheSolve() gives the inverse of this special type of matrix
## cached if already computed or its computed and cached for future use

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(inv){
                m <<- inv
        }
        getInv <- function(){
                m
        }
        list(set = set, get = get, setInv =  setInv, getInv =  getInv)
}




cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)){
                return(m)
        }
        dat <- solve(x$get(), ...)
        x$setInv(m)
        dat
}
