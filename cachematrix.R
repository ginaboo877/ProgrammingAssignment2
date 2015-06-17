## This function creates a special matrix object that can cache its inverse

## 1.	Sets up the matrix
## 2.	Retrieves the matrix
## 3.	Calculates the inverse – solve(x)
## 4.	Retrieves the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setmat <- function(y) {
        x <<- y
        inv <<- NULL
        }
        getmat <- function() x
        setinv <- function(inverse) inv <<-inverse
        getinv <- function() inv
        list(setmat = setmat, getmat = getmat,
        setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the matrix returned from makeCacheMatrix above.  
## If it's already been calculated, it retrieves the cached inverse

## x is the output from makeCacheMatrix 
## return the inverse of x

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
	##check if inverse has already been calculated – if so, return it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
	## if inverse not stored, calculate it
       data.mat <- x$getmat()
        inv <- solve(data.mat, ...)
	#store it for next time
        x$setinv(inv)
        return(inv)
}

