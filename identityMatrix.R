identityMatrix<-function(row = 2, col = 2){
  I<-matrix(nrow = row, ncol = col)
  for(i in 1:col){
    for(j in 1:row){
      if(i == j){
        I[i, j] = 1
      }
      else{
        I[i, j] = 0
      }
    }
  }
  return(I)
}