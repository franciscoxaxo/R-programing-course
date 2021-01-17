source("~/R/Curso/Matriz inversa/identityMatrix.R")

inverse<-function(x){
  
  col<- ncol(x) #columnas
  row<- nrow(x) #filaS
  
if(col == row){ #Mecanismo de control para matrices del tipo nxn
  I <- identityMatrix(col, row) #Funcion auxiliar que genera una matriz identidad del mismo tamaño
  
  GJ<- cbind(x, I) #Para resolver este problema por el metodo de Gauss-Jordan se extiende nuestra matriz uniendola a la matriz identidad

  for(j in 1:col){ #primera iteracion recorre las columnas
    i = row #fija un valor para i antes del ciclo anidado
    while(i > 0){ #ciclo anidado que recorre las filas
      if(j == i){ #compara los indices para no afectar la diagonal principal
        if(GJ[j, j] < 0){ # tras la ultima correccion ya no se hizo necesaria esta parte pero la mantengo para no afectar la integridad del codigo
          GJ[i, ]  = -GJ[i, ]
        }else{
          GJ[i, ]  = GJ[i, ]
        }
      }else{
        GJ[i, ] = GJ[j, ] - GJ[i, ]*(GJ[j, j]/GJ[i, j]) #En caso de no pertenecer a la diagonal se busca que el valor sea 0
      }
      i = i - 1 #la iteración while trabaja haciendo un recorrido hacia atras
    }
  }
  for(k in 1:col){# este ciclo utiliza la misma logica anterior 
    m = row
    while(m > 0){
      if(k == m){
        GJ[k, ] = GJ[k, ]/GJ[m, m] #se busca simplificar la diagonal para que valga 1
      }
      m = m - 1
    }
  }
  
  inverse = matrix(ncol = col, nrow = row) #genera matriz vacia
  for(o in 1:col){
    inverse[, o] = GJ[, (o+col)] #Elimina las columnas que ahora contienen a la matriz identidad
  }
  
  return(inverse) #devuelve la matriz inversa
  #no se creo un mecanismo de control para matrices con determinante nulo
}else{
  print("No es posible determinar la inversa de esta matriz") #en caso de no ser matriz nxn entrega este mensaje
}
}