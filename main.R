#Aca solo cambie detalles de los ejemplos entregados
#Las funciones principales fueron generadas en archivos separados

source("https://raw.githubusercontent.com/franciscoxaxo/R-programing-course/main/identityMatrix.R")#importa funcion que genera matriz identidad
source("https://raw.githubusercontent.com/franciscoxaxo/R-programing-course/main/inverse.R") #importa funcion que genera la inversa

makeMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}
