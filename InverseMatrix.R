makeCacheMatrix <- function( x = matrix() )
{
	Inv <- NULL
	set <- function(y)
		   {
				x <<- y
				Inv <<- NULL
		   }  ## End of function(y)
	get <- function() x
	setInverse <- function(Inverse) Inv <<- Inverse
	getInverse <- function() Inv
	list( set=set, get=get, setInverse=setInverse, getInverse=getInverse )
}

cacheSolve <- function( x, ... )
{
	Inv <- x$getInverse()
	if ( !is.null(Inv) )	
	{	
		message("Fetching data from cache !!!")
		return(Inv)
	} ## End of if ( !is.null(Inv) )	
	
	data <- x$get()
	
	Inv <- solve(data)
	x$setInverse(Inv)
	Inv
}
