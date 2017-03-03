####### Functions descriptions 

# makeCacheMatrix - function creates a matrix and sets inverse calculation in cache
# cacheSolve - function computes inverse of matrix



#The above function creates a matrix, sets inverse calculation in cache,
# and returns a lists of functions

makeCacheMatrix <- function(x = matrix()) {
               # i holds the cached value. Since at the start nothing is cached, i is setto NULL.
               i <- NULL
               #Store a matrix called "set". The argument y of that matrix is assigned to x, where
               # x is an object in an environment that is diferent from the current environment
               set <- function(y) {
                              x <<- y
                              #Since the matrix is assigned a new value, set again the cache value i 
                              i <<- NULL
               }
               
               # Returns the stored matrix "set"
               get <- function() x
               
               #Cache the given argument: "inverse"
               setinverse <- function(inverse) i <<- inverse
               
               #Get the cached value
               getinverse <- function() i
               
               #Return a list, where each element is a function.
               list(set = set,
                    get = get,
                    setinverse = setinverse,
                    getinverse = getinverse)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
               #Get the cached value
               i <- x$getinverse()
               
               #If a cached value exists return it, and display the message "getting cached data". 
               if (!is.null(i)) {
                              message("getting cached data")
                              return(i)
               }
               
               #Otherwise (cached value = NULL), calculate the inverse and store it in the cache:
               mat <- x$get()
               i <- solve(mat, ...)
               
               #Set the calculated inverse matrix on cache
               x$setinverse(i)
               i
               # Return a matrix that is the inverse of 'x'
               return (i)
}


