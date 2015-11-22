
## 1. create a special "matrix" object that can cache its inverse.
## 2. compute the inverse of the special "matrix" object
## If the inverse has already been calculated (and the matrix has not changed),
##then the inverse of the "matrix" object should be retrieved from the cache.

# creates a special "matrix" object
# that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {     
        inv <- NULL #sets the initial value of the inverse matrix to Null
        set <- function(y) {  #sets the value of the matrix
                x <<- y
                inv <<- NULL
        } 
        get <- function() x  #gets the value of the matrix
        setinverse <- function(solve) inv <<- solve  #sets the value of the inverse of the matrix
        getinverse <- function() inv  #gets the value of the inverse of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)   #returns the list of functions specified 
}

#computes the inverse of the special "matrix"
#returned by makeCacheMatrix above
CacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #If the inverse has already been calculated 
        #(and the matrix has not changed), then cachesolve 
        #retrieves the inverse from the cache.
        inv <- x$getinverse()  
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)     
        }    

        #If the inverse has not been cached already, calculate and returns the inverse of the matrix 
        data <- x$get()     
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
