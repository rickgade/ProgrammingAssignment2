## These functions take a matrix and create the inverse matrix.
## If a new matrix is not created, the inverse matrix that has
## already be calculted (and saved in cache) is used to save
## processing time.

## The function makecachMatrix has embedded functions to set (or
## create) a matrix, get the already present matrix, set the
## inverse matrix, or get the already present inverse matrix.

makeCacheMatrix <- function(x = matrix()) {   
        inverse <- NULL         #The inverse matrix is set to null.
        set <- function(y) {    #set function will create
                x <<- y         #universal matrix of imput matrix
                inverse <<- NULL #and make the inverse matrix
        }                       #null to be recalculated.
        get <- function() x     #get function returns input matrix.
        set_inverse <- function(inv) inverse <<- inv
                #set_inverse will manually put in inverse matrix
                #should not be used, except by cacheSolve funtion.
        get_inverse <- function() inverse
                #get_inverse outputs cached inverse matrix.
        list(set = set, get = get, #list of functions for further
             set_inverse = set_inverse, #use from command line or
             get_inverse = get_inverse) #other functions.
}


## This function checks to see if there is already an inverse
## matrix.  If there is already one available, it will not
## recalculate it.  If there is not already an inverse matrix,
## it will be calculated.

cacheSolve <- function(x, ...) {
        inverse <- x$get_inverse()
                #Gets the present inverse matrix.
        if(!is.null(inverse)) { #If it's present, returns matrix.
                message("getting cached data")
                return(inverse)
        }
        message("New matrix, so calculating inverse matrix.")
        data <- x$get()         #Since no inverse matrix present,
        inverse <- solve(data, ...)    #the inverse is calculated.
        x$set_inverse(inverse)  #Then the inverse is set into
        inverse                 #cache.
}
