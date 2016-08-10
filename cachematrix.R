## Two functions to calculate the inverse(solve) of a matrix, and cache the result for the future

## This first function create the matrix with it`s "constructors" (idk if it`s a correct term) 
## where have functios that:
##
## 1- set the value of the matrix (set)
## 2- return the value of the matrix (get)
## 3- set the value of the inverse/solve (setinv)
## 4- return the value of the inverse/solve (getinv)


makeCacheMatrix <- function(x = matrix()) {
		inv<-NULL
		set<- function(y){
			x<<-y
			inv<<-NULL
		}
		get <- function() x
		setinv <- function(x) inv <<- x
		getinv <- function() inv
		list(set=set,get=get,
			setinv=setinv,
			getinv=getinv)
}


## This function get a matrix created at the last function (makeCacheMatrix()) and check if it already
## has a solve value of it cached, if not,  this function calculate the inverse and store in cacheSolve

cacheSolve <- function(x, ...) {
        inv<-x$getinv()
		if(!is.null(inv)){
			message("getting cached data")
			return(inv)
		}
		data<-x$get()
		inv<-solve(data,...)
		x$setinv(inv)
		inv
}
