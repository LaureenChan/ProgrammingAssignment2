## Put comments here that give an overall description of what your
## functions do: 
#makeCacheMaxtrix: function that stores inverse matrix in a list
#cacheSolve: computes the cached matrix or retrieves inverse from the cache
makeCacheMatrix <- function(x = matrix()) {
        myinversematrix<-NULL
        set<- function(y){#set the value of the matrix
            x<<-y
            myinversematrix<<-NULL
        }
        get<-function() x #get the value of the matrix
        setinverse <-function(solve) myinversematrix<<-solve #set the value of inverse matrix
        getinverse <-function() myinversematrix #retrieves the value of the inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) #creates a list of the functions

}


#cacheSolve: computes the cached matrix or retrieves inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mymatrixinverse<-x$getinverse()
        if(!is.null(myinversematrix)){#if the inverse matrix value is not empty it will retrieve the cache
            message('getting cached data')
            return(myinversematrix)
        }
        data<-x$get()#retrieve the inverse matrix from from makeCacheMatrix 
        myinversematrix<-solve(data,...)#calculates inverse matrix
        x$setinverse(myinversematrix)
        myinversematrix #prints out the value of my inverse matrix
}
