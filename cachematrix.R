## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Esta funciòn veirfica si se ha calculado previamente la inversa de una matriz 
# Si no lo ha hecho, la calcula y la almacena para un uso posterior

makeCacheMatrix <- function(x = matrix()) {
  
  # inicializa la variable
  matriz_inversa <- NULL
  
  # Creamos una función que guarde la matriz inicial y la matriz cero 
  establece_matriz <- function(y) {
    x <<- y
    matriz_inversa <<- NULL
  }
  
  
  retorna_matriz <- function() return(x)
  guardar_matriz_inversa <- function(a) return(matriz_inversa <<- a)
  retornar_matriz_inversa <- function() return(matriz_inversa)
  
  ## Hacemos una lista con los resultados de las funciones
  # Establece la matriz
  # Recupera la matriz
  # Establece la inversa
  # Recupera la inversa
  list(
    establece_matriz = establece_matriz, 
    retorna_matriz = retorna_matriz, 
    guardar_matriz_inversa = guardar_matriz_inversa, 
    retornar_matriz_inversa = retornar_matriz_inversa
  )
  
}


## Write a short comment describing this function
# Verifica en primer lugar si no hay una matriz almacenada en la cache
# Si està almacenada, devuelve lo almacenado
# Si no hay nada almacenado, calcula la inversa

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  matriz_inversa <- x$retornar_matriz_inversa()
  
  if(!is.null(matriz_inversa)) {
    message("Retornando matriz en cache")
    return(matriz_inversa)
    
  }
  
  # Calcula la matriz Inversa
  matriz <- x$retorna_matriz()
  matriz_inversa <- solve(matriz)
  
  # Guarda la matriz inversa
  x$guardar_matriz_inversa(matriz_inversa)
  matriz_inversa
  
}