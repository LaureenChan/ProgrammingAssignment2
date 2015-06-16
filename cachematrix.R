## Put comments here that give an overall description of what your
## functions do: 
#makeCacheMaxtrix: function that stores inverse matrix
#cacheSolve: computes the cached matrix or retrieves inverse from the cache

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        mymatrixinverse<-NULL
        set<- function(y){#set the value of the matrix
            x<<-y
            mymatrixinverse<<-NULL
        }
        get<-function() x #get the value of the matrix
        setinverse <-function(solve) mymatrixinverse<<-solve #set the value of matrix
        getinverse <-function() mymatrixinverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mymatrixinverse<-x$getinverse()
        if(!is.null(mymatrixinverse)){
            message('getting cached data')
            return(mymatrixinverse)
        }
        data<-x$get()#retrieve the cached matrix from makeCacheMatrix 
        mymatrixinverse<-solve(data,...)
        x$setinverse(mymatrixinverse)
        mymatrixinverse
}
