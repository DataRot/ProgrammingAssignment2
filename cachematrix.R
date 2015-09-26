
## Data Rot Productions**********************************************************
##                                                                              *
##      Functions to utilize lexical scoping to cache and reference             *
##      large matrix computations.                                              *
##                                                                              *
##                                                                              *
## ******************************************************************************
##                              Edit History                                    *
##*******************************************************************************
##
##      21/09/2015  -   Begin development of function(s). 
##                      Study tutorial. Load Git hub template.
##
##      22/09/2015  -   Create and test functions. Add comments
##
##      23/09/2015  -   Add more comments, format comments.
##
## ******************************************************************************
##                              Functions descriptions:                         *
##*******************************************************************************
##                                                                              *
##      makeCacheMatrix function :                                              *
##                                                                              * 
##      This function creates a special "matrix" object that can cache its      *
##      inverse.                                                                *
##                                                                              *
##      Will create and manipulate a cached object holding the results of       *
##      a square invertable matrix. All subfunctions are made available and     *
##      can be called using the names given in the returned list once a new     *
##      instance of the makeCacheMatrix is created .                            * 
##                                                                              *
##              ie;  myfunc <- makeCacheMatrix()                                *
##                                                                              *
##      subfunctions defined :                                                  *
##                                                                              *
##              setmatrix(mat_x = matrix)                                       * 
##                                                                              *
##      matrix is initially set using the notation   :    myfunc$setmatrix(x)   *
##                                                                              *
##      matrix can be retrieved using                :    myfunc$getmatrix()    *
##                                                                              *
##                                                                              *
##      inverse matrix is set to the passed in value :    myfunc$setinverse(x)  *
##                                                                              *
##      inverse matrix is returned                   :    myfunc$getinverse()   *
##                                                                              *
##*******************************************************************************
makeCacheMatrix <- function(mat_x = matrix()) {
        
        mat_xInversed <- NULL 
        # Set the cached matrix to null, since new matrix received.
        setmatrix <- function(mat_y = matrix()) {
                mat_x <<- mat_y 
                mat_xInversed <<- NULL # set cached matrix to null at main function scope level.      
        }
        
        # Return the matrix
        getmatrix <- function() {
                return(mat_x)
        }        
        
        #  Set value of the cached matrix.
        setinverse <- function(inverse) {
                mat_xInversed <<- inverse 
        }
        
        # Return cached matix else return a null.
        getinverse <- function() {
                if (is.null(mat_xInversed)) {
                        message("First use cachSolve() to calculate the inverse.")
                        return(NULL)
                }
                return(mat_xInversed)
        }
        # Return all function names embedded in main function.
        return(list(setmatrix = setmatrix,getmatrix = getmatrix, 
             setinverse = setinverse, getinverse = getinverse))
}

##*****************************************************************************      
## cacheSolve:                                                                *        
##                                                                            *                                                                              
## This function computes the inverse of the special "matrix" returned by     *
## makeCacheMatrix above. If the inverse has already been calculated          *
## (and the matrix has not changed),then the cachesolve should retrieve       *
## the inverse from the cache.                                                *        
##                                                                            *        
##                                                                            *
##      Will calculate and return the inverse of a matrix.                    *        
##                                                                            *        
##      Both the matrix and inverse result are retrived and stored            *
##      using the makeCacheMatric functions.                                  *
##                                                                            *
##*****************************************************************************

cacheSolve <- function(x, ...) {
        
        # Get value of inversed matrix as part of passed in object.
        mat_inversed <- x$getinverse()
        
        # If cached value good then return it.
        if(!is.null(mat_inversed)) { 
                message("getting cached data")  
                return(mat_inversed)
        }
        
        # else create a new inversed matrix from input matrix value
        mat_data <- x$getmatrix()
        
        mat_inversed <- solve(mat_data, ...)
        x$setinverse(mat_inversed)
        return(mat_inversed)
}
