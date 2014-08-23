## Assignment 2 
## Bellow are two functions capable of generating a matrix, calculating it's inverse and caching the result for future usage.
## The main objective is to use R lexical scoping rules to reduce computation requirements through caching a results.

## This function basically generates a matrix with the arguments are provided by the user in "x = matrix( ???, nrow = ???, ncol =???).

makeCacheMatrix <- function (x = matrix()) {
        inv<-NULL					## This sets inv as NULL creating a Null object for the matrix inverse results
        set <- function(y){				##, this assures every time makeCacheMatrixis run the inverse is reset.
                x<<-y					## Here we are assingnig the result of the set function (Null matrix) in a way it can be
                inv <<- NULL				## accessed out of it's own environmet through the <<- operator
        }
        get <- function () {x}				## this function get's the matrix to be inversed
        setmatrix <- function(solve) {inv <<- solve} 	## this function set's the result of the matrix inversion
        getinvmatrix<- function() {inv}			## this function get's the inversed matrix
        list(set=set, get=get, setmatrix=setmatrix, getinvmatrix=getinvmatrix)       ## The output of this function is, therefore, a list of functions to be
}										     ## used by cacheSolve


## cacheSolve is capable of obtaining the matrix inversion or use a cached result, if the result has already being cached by a previous run.

cacheSolve <- function(x=matrix(), ...){		##The argument of this function are the stored result of makeCacheMatrix (like in the test below)
        inv<- x$getinvmatrix()				## this gets the inv object
        if (!is.null(inv)){				## if 'inv' is different from NULL the function will return the cached inversed matrix
                message("getting cached inverse matrix")
                return(inv)
        }
        matrix <- x$get()				## but if 'inv' is equal to NULL then the funcion will solve the specified matrix 
        inv <- solve(matrix,...)			## and cache the results through 'setmatrix' function
        x$setmatrix(inv)
        inv
}

## Below are the codes I've used for checking makeCacheMatrix and cacheSolve working together.
## You can set the size of the matrix using ncol and nrow arguments and adjusting the the range of rnorm.

test<-makeCacheMatrix(matrix(rnorm(1:100),10,10))
cacheSolve(test)					## Run twice to check caching 