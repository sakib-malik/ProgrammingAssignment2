## make cache matrix creates a special type of matrix 
## that have various functions like set , get, setInv, getInv
## to cache , get and update its inverse and values
## cacheSolve() gives the inverse of this special type of matrix
## cached if already computed or its computed and cached for future use



## this creates a special type of matrix with above mentioned functions
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

## this functions returns the inverse of the special matrix
## if already computed it returns the cached value otherwise compute it afresh
## and store for future use
cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)){
                return(m)
        }
        dat <- solve(x$get(), ...)
        x$setInv(m)
        dat
}
