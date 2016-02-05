## function makeCacheMatrix makes cache of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
                m<-NULL
                s<-function(y){
                        x<<-y
                        m<<-NULL
                }
                g<-function() x
                setmatr<-function(solve) m<<- solve
                getmatr<-function() m
                list(s=s, g=g,
                     setmatr=setmatr,
                     getmatr=getmatr)
        }
        


## cacheSolve calculates inverse matrix.
##if inverse matrix calculated earlier then give message "getting cached data"
## and give inverse matrix from cache without calculation

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatr()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$g()
        m<-solve(matrix, ...)
        x$setmatr(m)
        m
}

