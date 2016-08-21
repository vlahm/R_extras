die<-1:6

roller<-function(times){
  results<-sample(die,times,replace=TRUE)
  return(results)
}

roller(5)
