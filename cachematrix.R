# RPAssignment2

## makeCacheMatrix function is built to save a matrix and cache its inverse.
## Then it would return a special "matrix", which is really a list of functions.

makeCacheMatrix <- function(x = matrix()) {

	#The variable 'inv' caches the inverse of the matrix.
	#But initially nothing is cached
	inv<-NULL
	
	#This function set the matrix
	setMatrix<-function(newMatrix){
		x<<-newMatrix
		inv<<-NULL
	}

	#This function get the matrix
	getMatrix<-function() x

	#This function set the inverse of the matrix
	setinverse<-function(inverse) inv<<-inverse

	#This function get the inverse of the matrix
	getinverse<-function() inv

	list(setMatrix = setMatrix, getMatrix = getMatrix,
	     setinverse=setinverse,
	     getinverse=getinverse)
}



## The following function calculates the inverse of the special "matrix" created with the above function.
#However, it first checks to see if the inverse has already been calculated.
#If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse of the data 
#and sets the inverse of the matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
	
	## Return the cached inverse
	inv<-x$getinverse()
	
	#If a cached inverse exists return it
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	
	##Otherwise:
	
	#Get the matrix and save it in 'mat'
	mat<-x$getMatrix()
	
	#Compute the inverse of the matrix
	inv<-solve(mat)
	
	#Cache the inverse computed above
	x$setinverse(inv)
	
	#Return the inverse
	inv
}

#José M. Mireles :)
