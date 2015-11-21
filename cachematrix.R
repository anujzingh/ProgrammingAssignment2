## There are two functions: makeCacheMatrix and cacheSolve
## Functions together gives output as inverse of a matrix
## Call is to be made to cacheSolve function by user
## Matrix for which inverse is to be calculated is passed as an argument to cacheSolve function  
## cacheSolve function interacts with makeCacheMatrix function and returns inverse of matrix
## Inverse of matrix is retrieved from cache in case it is already stored
## In case matrix is already not stored, it is calculated, stored in cache and retrieved


## makeCacheMatrix comprises of four functions
## set function is to store the matix
## get function is to retrieve the matrix
## setinv function is to set the inverse of input matrix
## getinv function is to retrieve the inverse of matrix
## function returns a list comprising these functions
## In this function, input matrix is compared to stored matrix
## If the same matrix exists, it's inverse is returned from the cached
## If the same matrix is not present, it's inverse is calculated, stored in cache and returned
ab
makeCacheMatrix <- function(ma = matrix()){
	inv <- NULL			  ## Initiating inverse to NULL
	set <- function(m){	  ## set function is to store the Input matrix
		ma <<- m            ## store matrix in cache
		inv <<- NULL        ## inverse is NULL yet
	}

	get <- function(){
		ma                  ## return stored matrix
	}                         ## incase matrix is not stored, NULL wold be returned

	setinv <- function(){
		inv <<- solve(ma)   ## calculate inverse of matrix and store it in cache
	}

	getinv <- function(){
		inv                 ## return stored inverse of the matrix
	}

	list(set=set,get=get,setinv=setinv,getinv=getinv)   ## list comprising set, get, setinv, getinv functions is returned
}

g <- makeCacheMatrix()          ## Creating object of makeCacheMatrix function

## cacheSolve function takes Input matrix for which inverse is to be calculated and Object of makeCacheMatrix as argument
## Compares the Input matrix with the matrix stored in the cache
## If the matrices are same, makes call to getinv function in makeCacheMatrix and retrieves inverse stored in cache
## If the matrices are not same or the matrix doesn't exist, then makes call set, setinv and getinv functions in makeCacheMatrix to calculate inverse and store in cache and retrieve the same

cacheSolve <- function(n,x=g){  ## n is the input matrix and X is passed the object of makeCacheMatrix function
	y <- x$get()              ## call is made to get function in makeCacheMatrix function
	if(is.matrix(n) && is.matrix(y) && dim(n) == dim(y) && all(n == y)){      ## Check whether the matrix stored in the cache is same as Input matrix
		print("The inverse is present in cache")  ## shows the same matrix is present in the cache
		z <- x$getinv()                           ## call is made to getinv function in makeCacheMatrix to get the inverse of matrix from cache
		print("caching inverse")                  ## show user that result is being retrieved from cache
		print("Inverse from cache is")            ## showing value from cache
		return(z)                                 ## return the value obtained from  cache and exit the function
	}
	print("The inverse is not in Cache")            ## In case the Input matrix is not in cache, show user the message for the same
	print("Calculating inverse and storing it in cache")  ## show that inverse is to be calculated and then it will be stored in cache
	k <- x$set(n)						## call set function in makeCacheMatrix to store the Input matrix in the cache
	l <- x$setinv()						## call setinv function in makeCacheMatrix to calculate inverse of matrix and store it in cache
	p <- x$getinv()                                 ## call to getinv function in makeCacheMatrix to get the calculated inverse of matrix
	print("Inverse has been stored in Cache")		## Show that newly calculated inverse has been stored in cache
	print("Just calculated inverse is")			## Show the value of inverse is just calculated
	return(p)							## return the newly calculated inverse of matrix
}