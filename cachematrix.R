## The code consists of functions to store the matrix and its inverse
## when new matrix is received it recalculates the inverse and cache it
## otherwise the cache inverse is retrived

## makeCacheMatix function creates list of four functions 
## these functions when called from another function will set or get the matrix vector
## or set or get the inverse of the matix vector

makeCacheMatrix <- function(x = matrix()) {
        invMat<-NULL
        ##function to set a new matrix and set its invMat variant to NULL
        setMat<-function(y){
                if(matrixEqual(x,y)==TRUE){
                        print("matrix is same")
                } 
                else{
                        print("changing matrix")
                        x<<-y
                        invMat<<-NULL     
                }
        }
        ##function to get the stored mat
        getMat<-function()x
        
        ##Func to store the inverse of matix
        setInvMat<-function(inverseMatrix) invMat<<-inverseMatrix
        
        ##fun to retrive the stored inverse matrix
        getInvMat<-function()invMat
        ##preparing all the list of functions
        list(setMat=setMat,getMat=getMat,
             setInvMat=setInvMat,getInvMat=getInvMat)

}


## cacheSolve func will retrive the inverse if matrix is same or new
## otherwise it recalculates


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(x$getInvMat()))
           {
           print("getting cached inverse of matrix")
           return(x$getInvMat())
        }
        print("new matrix or inverse is null")
        newMat<-x$getMat()
        invMat<-solve(newMat)
        x$setInvMat(invMat)
        invMat
}

## matrixEqual function checks for equality of two matrix and returns a logical value 
matrixEqual <- function(a, b){ 
        is.matrix(a) && is.matrix(b) && dim(a) == dim(b) && all(a == b)
}
