####### Functions descriptions 

# makeCacheMatrix - function creates a matrix and sets inverse calculation in cache
# cacheSolve - function computes inverse of matrix



#The above function creates a matrix, sets inverse calculation in cache,
# and returns a lists of functions

makeCacheMatrix <- function(x = matrix()) {
               # "cache" holds the cached value. Since at the start nothing is cached, "cache" is set to NULL.
               cache <- NULL
               #Store a matrix called "set". The argument "newValue" of that matrix is assigned to x, where
               # x is an object in an environment that is diferent from the current environment
               set <- function(newValue) {
                              x <<- newValue
                              #Since the matrix is assigned a new value, set again the cache value "cache" 
                              cache <<- NULL
               }
               
               # Returns the stored matrix "set"
               get <- function() x
               
               #Cache the given argument: "inverse"
               setinverse <- function(inverse) cache <<- inverse
               
               #Get the cached value
               getinverse <- function() cache
               
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
               cache <- x$getinverse()
               
               #If a cached value exists return it, and display the message "getting cached data". 
               if (!is.null(cache)) {
                              message("getting cached data")
                              return(cache)
               }
               
               #Otherwise (cached value = NULL), calculate the inverse and store it in the cache:
               mat <- x$get()
               cache <- solve(mat, ...)
               
               #Set the calculated inverse matrix on cache
               x$setinverse(cache)
               cache
               # Return a matrix that is the inverse of 'x'
               return (cache)
}


######## Testing the functions##################


#Lines starting with # are simple comments, lines starting with #> are things printed in the output.

# Create a *square* matrix (because `solve` only handles square matrices)

z <- makeCacheMatrix( matrix(c(1,2,3,4), nrow = 2, ncol = 2) )   #We can substitute c(1,2,3,4)) for 1:4

summary(z)


#>Length Class  Mode    
#>set        1      -none- function
#>get        1      -none- function
#>setinverse 1      -none- function
#>getinverse 1      -none- function


#  Return the stored matrix z
z$get()

#>       [,1] [,2]
#>[1,]    1    3
#>[2,]    2    4


# Get the inverse matrix stored (Since we dont have calculate and store the inverse matrix, we get NULL)

z$getinverse()

#>NULL


#Get the inverse matrix
cacheSolve(z)

#>       [,1] [,2]
#>[1,]   -2  1.5
#>[2,]    1 -0.5

#The  2nd time we run the function, we get the cache value 
cacheSolve(z)

#> getting cached data
#>      [,1] [,2]
#>[1,]   -2   1.5
#>[2,]    1  -0.5



