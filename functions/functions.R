


dataprep = function(ind,df,variable,horizon,add_dummy = TRUE, univar = FALSE, factonly = FALSE, nofact = FALSE)
{
  df=df[ind,]
  y=df[,variable]
  
  if(nofact==TRUE){
    if(univar==FALSE){
      x=df
    }else{
      x = as.matrix(df[,variable])
    }
  }else{
    if(univar==FALSE){
      factors=princomp(scale(df))$scores[,1:4]
      if(factonly == TRUE){
        x = cbind(df[,variable],factors)
      }else{
        x=cbind(df,factors)
      }
    }else{
      x = as.matrix(df[,variable])
    }
  }
  
  X=embed(as.matrix(x),4)
  
  Xin=X[-c((nrow(X)-horizon+1):nrow(X)),]
  Xout=X[nrow(X),]
  Xout=t(as.vector(Xout))
  yin=tail(y,nrow(Xin))
  
  if("2008-11-01" %in% names(yin)){
    
    dummy=rep(0,length(yin))
    intervention=which(names(yin)=="2008-11-01")
    dummy[intervention]=1
    if(add_dummy == TRUE){
      Xin=cbind(Xin,dummy)
      Xout=cbind(Xout,0)
    }
    
  }else{
    dummy = rep(0,length(yin))
    if(add_dummy == TRUE){
      Xin=cbind(Xin,dummy)
      Xout=cbind(Xout,0)
    }
  }
  
  return(list(dummy = dummy, Xin = Xin, Xout = Xout, yin = yin))
  
}

runlasso=function(ind,df,variable,horizon, alpha = 1, alpha2 = 1, adaptive = FALSE){
  
  prep_data = dataprep(ind,df,variable,horizon)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
  modelest = ic.glmnet(Xin,yin,alpha = alpha)
  if(adaptive==TRUE){
    classo = coef(modelest)
    penalty = (abs(classo[-1])+1/sqrt(length(yin)))^(-1)
    modelest = ic.glmnet(Xin,yin, penalty.factor = penalty, alpha = alpha2)
  }
  
  forecast=predict(modelest,Xout)
  
  ### outputs ###
  coeflvl=coef(modelest)[-1]
  coefpar=coeflvl*apply(Xin,2,sd)
  lambda=modelest$lambda
  outputs=list(coeflvl=coeflvl,coefpar=coefpar,lambda=lambda)
  
  return(list(forecast=forecast, outputs=outputs))
}

runar=function(ind,df,variable,horizon, type = "fixed"){
  prep_data = dataprep(ind,df,variable,horizon, univar = TRUE, add_dummy = FALSE)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  dummy = prep_data$dummy
  
  if(type=="fixed"){
    modelest=lm(yin~Xin+dummy)
    best = ncol(Xin)
  }
  
  if(type=="bic"){
    bb=Inf
    best = 1
    for(i in seq(1,ncol(Xin),1)){
      m=lm(yin~Xin[,1:i]+dummy)
      crit=BIC(m)
      if(crit<bb){
        bb=crit
        modelest=m
        best = i
      }
    }
  }
  coef=coef(modelest)
  coef[is.na(coef)] = 0
  forecast=c(1,Xout[,1:best],0)%*%coef
  
  return(list(forecast=forecast))
}

runrf=function(ind,df,variable,horizon){
  prep_data = dataprep(ind,df,variable,horizon)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
  modelest=randomForest::randomForest(Xin,yin, importance = TRUE)
  forecast=predict(modelest,Xout)
  
  ## outputs
  importance = randomForest::importance(modelest)
  outputs = list(importance = importance)
  
  return(list(forecast=forecast, outputs = outputs))
}

runrfols=function(ind,df,variable,horizon){
  prep_data = dataprep(ind,df,variable,horizon, add_dummy = FALSE)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  dummy = prep_data$dummy
  
  modelest=randomForest::randomForest(Xin,yin,keep.inbag = TRUE,maxnodes = 25,ntree=500)
  samples=modelest$inbag
  
  predaux=rep(0,ncol(samples))
  for(k in 1:ncol(samples)){
    saux=samples[,k]
    sboot=c()
    for(i in 1:length(saux)){
      sboot=c(sboot,rep(i,saux[i]))
    }
    xaux=Xin[sboot,]
    yaux=yin[sboot]
    tr=randomForest::getTree(modelest,k)
    selected=unique(tr[,3])
    selected=sort(selected[selected>0])
    modelols=lm(yaux~xaux[,selected]+dummy[sboot])
    cols=coef(modelols)
    cols[is.na(cols)]=0
    predaux[k]=c(1,Xout[selected],0)%*%cols
  }
  
  forecast=mean(predaux)
  
  return(list(forecast=forecast))
}

runadalassorf=function(ind,df,variable,horizon){
  prep_data = dataprep(ind,df,variable,horizon)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
  lasso = HDeconometrics::ic.glmnet(Xin,yin)
  classo = coef(lasso)
  penalty = (abs(classo[-1])+1/sqrt(length(yin)))^(-1)
  adalasso = HDeconometrics::ic.glmnet(Xin,yin, penalty.factor = penalty)
  
  selected=which(adalasso$coef[-1]!=0)
  modelest=randomForest::randomForest(Xin[,selected],yin)
  forecast=predict(modelest,Xout[selected])
  
  return(list(forecast=forecast))
}

runbagging=function(ind,df,variable,horizon){
  prep_data = dataprep(ind,df,variable,horizon)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
  
  modelest=bagging(Xin,yin,R=100,l=5,pre.testing = "group-joint")
  forecast = predict(modelest,Xout)
  
  ## outputs
  
  nselect=modelest$coefficients
  nselect[nselect!=0]=1
  nselect[is.na(nselect)]=0
  nselect=colSums(nselect)
  
  outputs = list(nselect = nselect)
  
  return(list(forecast=forecast, outputs = outputs))
}

runcsr=function(ind,df,variable,horizon){
  prep_data = dataprep(ind,df,variable,horizon)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
  indice = which(colnames(df)==variable)
  f.seq=seq(indice,ncol(Xin)-1,ncol(df)+4)
  modelest=csr(Xin,yin,fixed.controls =c(f.seq,ncol(Xin)))
  forecast = predict(modelest,Xout)
  
  ## outputs
  
  nselect=modelest$coefficients
  nselect[nselect!=0]=1
  nselect[is.na(nselect)]=0
  nselect=colSums(nselect)
  
  outputs = list(nselect = nselect)
  
  return(list(forecast=forecast, outputs = outputs))
}

runfact=function(ind,df,variable,horizon){
  prep_data = dataprep(ind,df,variable,horizon, factonly = TRUE, add_dummy = FALSE)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  dummy = prep_data$dummy
  
  bb=Inf
  for(i in seq(5,20,5)){
    m=lm(yin~Xin[,1:i]+dummy)
    crit=BIC(m)
    if(crit<bb){
      bb=crit
      modelest=m
      f.coef=coef(modelest)
      coefdum=f.coef[length(f.coef)]
      f.coef=f.coef[-length(f.coef)]
    }
  }
  coef=rep(0,ncol(Xin)+1)
  coef[1:length(f.coef)]=f.coef
  coef=c(coef,coefdum)
  coef[is.na(coef)]=0
  
  forecast=(cbind(1,Xout,0)%*%coef)[1]
  
  ## outputs
  
  outputs = list(coef = coef)
  
  return(list(forecast=forecast, outputs = outputs))
}

runtfact=function(ind,df,variable,horizon){
  
  dfaux = df[ind,]
  if("2008-11-01" %in% rownames(dfaux)){
    if(variable %in% c("CPI","PCE")){
      dummy=rep(0,nrow(dfaux))
      intervention=which(rownames(dfaux)=="2008-11-01")
      dummy[intervention]=1
    }
  }else{
    dummy = rep(0,nrow(dfaux))
  }
  
  index = which(colnames(dfaux)==variable)
  mat = cbind(embed(dfaux[,variable],5),tail(dummy,nrow(dfaux)-4),tail(dfaux[,-index],nrow(dfaux)-4))
  pretest=tfaux(mat,pre.testing="individual",fixed.controls = 1:4)[-c(1:6)]
  
  pretest[pretest!=0]=1
  aux = rep(0,ncol(dfaux))
  aux[index] = 1
  aux[-index] = pretest
  selected=which(pretest==1)
  dfreduced = df[,selected]
  
  prep_data = dataprep(ind,dfreduced,variable,horizon, add_dummy = FALSE, factonly = TRUE)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  dummy = prep_data$dummy
  
  bb=Inf
  for(i in seq(5,20,5)){
    m=lm(yin~Xin[,1:i]+dummy)
    crit=BIC(m)
    if(crit<bb){
      bb=crit
      modelest=m
      f.coef=coef(modelest)
      coefdum=f.coef[length(f.coef)]
      f.coef=f.coef[-length(f.coef)]
    }
  }
  coef=rep(0,ncol(Xin)+1)
  coef[1:length(f.coef)]=f.coef
  coef=c(coef,coefdum)
  coef[is.na(coef)]=0
  
  forecast=(cbind(1,Xout,0)%*%coef)[1]
  
  ## outputs
  
  outputs = list(coef = coef)
  
  return(list(forecast=forecast, outputs = outputs))
}

accumulate_model = function(forecasts){
  
  acc3 = c(rep(NA,2),sapply(1:(nrow(forecasts)-2), function(x){
    prod(1+diag(forecasts[x:(x+2),1:3]))-1
  })) 
  acc6 = c(rep(NA,5),sapply(1:(nrow(forecasts)-5), function(x){
    prod(1+diag(forecasts[x:(x+5),1:6]))-1
  }))
  acc12 = c(rep(NA,11),sapply(1:(nrow(forecasts)-11), function(x){
    prod(1+diag(forecasts[x:(x+11),1:12]))-1
  }))
  
  forecasts = cbind(forecasts,acc3,acc6,acc12)
  colnames(forecasts) = c(paste("t+",1:12,sep = ""),"acc3","acc6","acc12")
  
  return(forecasts)
  
}

ic.glmnet = function (x, y, crit = c("bic", "aic", "aicc", 
                                     "hqc"), alpha = 1, ...) 
{
  if (is.matrix(x) == FALSE) {
    x = as.matrix(x)
  }
  if (is.vector(y) == FALSE) {
    y = as.vector(y)
  }
  crit = match.arg(crit)
  n = length(y)
  model = glmnet(x = x, y = y, alpha = alpha,...)
  coef = coef(model)
  lambda = model$lambda
  df = model$df
  yhat = cbind(1, x) %*% coef
  residuals = (y - yhat)
  mse = colMeans(residuals^2)
  sse = colSums(residuals^2)
  nvar = df + 1
  bic = n * log(mse) + nvar * log(n)
  aic = n * log(mse) + 2 * nvar
  aicc = aic + (2 * nvar * (nvar + 1))/(n - nvar - 1)
  hqc = n * log(mse) + 2 * nvar * log(log(n))
  sst = (n - 1) * var(y)
  r2 = 1 - (sse/sst)
  adjr2 = (1 - (1 - r2) * (n - 1)/(nrow(x) - nvar - 1))
  crit = switch(crit, bic = bic, aic = aic, aicc = aicc, hqc = hqc)
  selected = best.model = which(crit == min(crit))
  ic = c(bic = bic[selected], aic = aic[selected], aicc = aicc[selected], 
         hqc = hqc[selected])
  result = list(coefficients = coef[, selected], ic = ic, lambda = lambda[selected], 
                nvar = nvar[selected], glmnet = model, residuals = residuals[, 
                                                                             selected], fitted.values = yhat[, selected], ic.range = crit, 
                df = df, call = match.call())
  class(result) = "ic.glmnet"
  return(result)
}

tfaux=function (mat, pre.testing = c("group-joint","joint","individual"), fixed.controls = NULL,
                t.stat = 1.96,ngroups=10)
{
  pre.testing=match.arg(pre.testing)
  y = mat[, 1]
  X = mat[, -1]
  if (pre.testing == "joint") {
    if (nrow(X) < ncol(X)) {
      stop("Error: Type = joint is only for data with more observations than variables")
    }
    m1 = lm(y ~ X)
    t1 = summary(m1)$coefficients[-1, 3]
    s1 = which(abs(t1) > t.stat)
    if (length(s1) == 0) {
      stop("Error: The pre-testing excluded all variables",
           "/n")
    }
  }
  if (pre.testing == "group-joint") {
    
    N=ncol(X)
    n=ceiling(N/ngroups)
    varind=1:N
    t1=rep(NA,N)
    for(i in 1:ngroups){
      selected=sample(order(varind),min(n,length(varind)),replace = FALSE)
      X0=X[,varind[selected]]
      m1=lm(y~X0)
      
      t0=rep(0,length(selected))
      aux=which(is.na(coef(m1)[-1]))
      t = summary(m1)$coefficients[-1, 3]
      if(length(aux)==0){
        t0=t
      }else{
        t0[-aux]=t
      }
      
      t1[varind[selected]]=t0
      varind=varind[-selected]
    }
    
    s1 = which(abs(t1) > t.stat)
    if (length(s1) == 0) {
      stop("Error: The pre-testing excluded all variables",
           "/n")
    }
  }
  if (pre.testing == "individual") {
    if (length(fixed.controls) > 0) {
      w = X[, fixed.controls]
      nonw = setdiff(1:ncol(X), fixed.controls)
    }
    else {
      w = rep(0, nrow(X))
      nonw = 1:ncol(X)
    }
    store.t = rep(NA, ncol(X))
    store.t[fixed.controls] = Inf
    for (i in nonw) {
      m1 = lm(y ~ X[, i] + w)
      t1 = summary(m1)$coefficients[2, 3]
      store.t[i] = t1
    }
    s1 = which(abs(store.t) > t.stat)
  }
  if (length(s1) > nrow(X)) {
    stop("Error: The pre-testing was not able to reduce the dimension to N<T")
  }
  m2 = lm(y ~ X[, s1])
  final.coef = rep(0, ncol(X))
  final.coef[s1] = coef(m2)[-1]
  names(final.coef) = colnames(X)
  final.coef = c(coef(m2)[1], final.coef)
  return(final.coef)
}
