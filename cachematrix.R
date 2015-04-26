## The following functions calculate the inverse of a matrix and saves it
## to the cache such that the next time the user attempts to calculate the
## matrix inverse, the previously saved value is returned instead of
## repeating the calculation.

## This function creates a special "matrix" object, which is a list 
## containing a function to:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
       ## create a matrix object
        
        m<-NULL
        set<-function(y){
                ## assign the input matrix y to the variable x
                x<<-y 
                ## re-initialize m
                m<<-NULL
        }
        ## return the matrix
        get<-function() x
        ## set m to the inverse of the matrix
        setinvmatrix<-function(solve) m<<- solve
        ## return the inverse of the matrix 
        getinvmatrix<-function() m
        list(set=set, get=get,
             setinvmatrix=setinvmatrix,
             getinvmatrix=getinvmatrix)
        
}


## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse
## has already been caclulated. If so, it 'get's the inverse from the cache
## and skips the computation. Otherwise, it calculates the matrix inverse
## and sets the value of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
        ## Return the inverse of of the matrix
        m<-x$getinvmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setinvmatrix(m)
        m
}
