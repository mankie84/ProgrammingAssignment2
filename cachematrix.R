## makeCacheMatrix caches a matrix object and list of functions that 
allow to store the inverse and re-set the original objext. CacheSolve 
computes the inverse of that matrix and caches the result.

## makeCacheMatrix takes arguments which define a matrix, in accordance 
with the R <matrix> function conventions (z=vector of content for 
matrix, x=number of rows, y= number of columns). The function checks 
that the arguments form an invertible matrix and cache it. Additional 
comments arer included at the end of each line of code.

makeCacheMatrix <- function(z = numeric(), x = numeric(), y = numeric()) 
{

if (missing(x) || missing(y) || missing(z)) {		## just checking that this gives a square matrix
	print("Please define a square matrix as function argument.")
} else{

if (x==0 || z==0 || y==0 || x != y) {			## just checking that this gives a square matrix
	print("Please define a square matrix as function argument.")
} else {

spc_matrix <- matrix(z,x,y)				## creating the "special matrix"
inv <- NULL						## setting the variable that records if inverse has been computed to 
NULL

set <- function(zz,xx,yy) {				## enable user to re-set matrix stats quickly
	x <<- xx
	z <<- zz
	y <<- yy	
	inv <<- NULL
	spc_matrix <<- matrix(zz,xx,yy)
}

get <- function() spc_matrix				## define how to get the matrix
setinverse <- function(solve) inv <<- solve		## define how solve is saved in <inv> independent of environment
getinverse <- function() inv				## define how to pull the inv variable

list(set = set, get = get,				## put all functions into a neat list
	setinverse = setinverse,
	getinverse = getinverse)
}}}


## cacheSolve computes the inverse of a matrix object passed to it as 
argument. It then stores the result and pulls it from cache if re-invoked. Additional 
comments included at the end of each line of code.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()					## pull <inv> out of the functions list
        if(!is.null(inv)) {					## define what to do if solve has already been computed
                message("getting cached data")
                return(inv)
        }
        data <- x$get()						## define <data> and get content from matrix object
        inv <- solve(data, ...)					## define that <inv holds> <solve> of the matrix
        x$setinverse(inv)					## pull inv	
        inv							## print <inv>
}
