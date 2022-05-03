rolling_window=function(fn,df,nwindow=1,horizon,variable,...){
  ind=1:nrow(df)
  window_size=nrow(df)-nwindow
  indmat=matrix(NA,window_size,nwindow)
  indmat[1,]=1:ncol(indmat)
  for(i in 2:nrow(indmat)){
    indmat[i,]=indmat[i-1,]+1
  }
  rw=apply(indmat,2,fn,df=df,horizon=horizon,variable=variable,...)
  forecast=unlist(lapply(rw,function(x)x$forecast))
  outputs=lapply(rw,function(x)x$outputs)
  return(list(forecast=forecast, outputs=outputs))
  
}