
#### gets out of sample y and computes random walk forecasts ###
library(roll)
load("data/data.rda")
dates = data$date
data = data%>%select(-date)%>%as.matrix()
rownames(data) = as.character(dates)

nwindows = 180

y = data[,"CPIAUCSL"]
y = cbind(y,roll_prod(1+y,3)-1,roll_prod(1+y,6)-1,roll_prod(1+y,12)-1)
yout = tail(y,nwindows)

rw = matrix(NA,nwindows,12)
for(i in 1:12){
  aux=data[(nrow(data)-nwindows-i+1):(nrow(data)-i),"CPIAUCSL"]
  rw[,i]=aux;
}

rw3 = tail(embed(y[,2],4)[,4],nwindows)
rw6 = tail(embed(y[,3],7)[,7],nwindows)
rw12 = tail(embed(y[,4],13)[,13],nwindows)
rw = cbind(rw,rw3,rw6,rw12)
colnames(rw) = c(paste("t+",1:12,sep = ""),"acc3","acc6","acc12")

save(yout,file = "forecasts/yout.rda")
save(rw,file = "forecasts/rw.rda")



