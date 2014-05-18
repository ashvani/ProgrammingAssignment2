

### To compute the inverse of matrix x(say) these two function can be jointly used as 
### mySolve <- function(x, ...){
###         List <- makeCacheMatrix(x)
###         cacheSolve(List)
###}
### now call mySolve(x)

# function makeCacheMatrix returns a list of four functions namely set, get, setInverse, getInverse.
# function set caches matrix using assingment operator <<-.
# function get returns matrix.
# function setInverse caches inverese of matrix using operator <<-.
# function getInverse returns inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
        Result <- NULL
        set<- function(y){
                x <<- y
                Result <<- NULL # 'Result' is the inverse of matrix which is kept as NULL initially
        }                       
        get <- function() x  # function get returns input matrix
        setInverse <- function(Inverse)
                Result <<- Inverse  # function setInverse sets inverse of matrix in variable Result
        getInverse <- function() Result
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
       # function makeCacheMatrix returns a list having four above functions 
}


 ## function cacheSolve takes a list object x and arguments via '...' 
 ## this function first checks that inverse of matrix is already calculated or not 
 ## if inverse is already calculated then function getInverse returns a non-NULL value i.e.
 ## inverse of input matrix, otherwise if getInverse returns NULL then function cacheSolve calls gets 
 ## matrix, sets inverse of this in cache and returns inverse .

cacheSolve <- function(x, ...) {
        Inv <- x$getInverse()
        if(!is.null(Inv)){ # checks that getInverse function returns null or not  
                return(Inv) # if getInveres returns non-NULL
        }
        ## getInverse returns NULL i.e. matrix x is encountered first time
        mat <- x$get()
        Result <- solve(mat)
        x$setInverse(Result)
        Result
}


 #### END OF R PROGRAM ####
