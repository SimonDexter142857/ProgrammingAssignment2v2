#################################################
# Author:   Simon Dexter, simondex@yahoo.com
# Date:     09/19/2014
#
# Description: 
#           Cache matrix programming assignment
#################################################


#---------------------------------------------
# Returns a special matrix object which maintains 
# its inverse in cache
#---------------------------------------------


makeCacheMatrix <- function(x = matrix()) {
	
	# storage for cache
	inv = NULL;
      # actual matrix
	m = NULL;

      if (!is.null(x) ) m = x;

	# setting matrix data and initializing
	# inverse matrix to null
	set = function(y) {
		m <<- y; 
		inv <<- NULL;
	}

	
	# returning matrix data
	get = function () m;
	
	# setting the inverse if not computed yet
	setInverse = function(inverse) inv <<- inverse;
	

	# getting the inverse 
	getInverse = function() inv; 
	
	# creating and returning the 
	# resultant object
	list (set = set, get = get, setInverse = setInverse, getInverse = getInverse);
	
}



#---------------------------------------------
# Retrieves inverse of matrix contained 
# in an object returned by makeCacheMatrix
# function, otherwise computes the inverse
# and stores in the object
#---------------------------------------------

cacheSolve <- function(x, ...) {
      # retrieving the inverse 
      # from the object
      inv = x$getInverse();
	
      # the inverse is already computed 
      if (!is.null (inv)) {
           message ("getting cached data"); 
           return(inv); 
      }
       # getting to the data to compute
       # the inverse 
       data = x$get();
       # computing the inverse 
       i = solve(data, ...);
       # storing the inverse in the object 
       x$setInverse (i);

       ## Return a matrix that is the inverse of 'x'	
       i;
        
}
