
### must add package for specific models ###
# library(devtools)
# install_github("gabrielrvsc/HDeconometrics")
library(HDeconometrics)
library(glmnet)
library(randomForest)

source("functions/rolling_window.R")
source("functions/functions.R")

#####
## The file with the forecasts will be saved with model_name
model_name = "RF"
## The function called to run models is model_function, which is a function from functions.R
model_function = runrf
#####


load("data/data.rda")
dates = data$date
data = data%>%select(-date)%>%as.matrix()
rownames(data) = as.character(dates)

####### run rolling window ##########
nwindows = 180
model_list = list()
for(i in 1:12){
  model = rolling_window(model_function,data,nwindows+i-1,i,"CPIAUCSL")
  model_list[[i]] = model
  cat(i,"\n")
}

forecasts = Reduce(cbind,lapply(model_list, function(x)head(x$forecast,nwindows)))

forecasts = accumulate_model(forecasts)

save(forecasts,file = paste("forecasts/",model_name,".rda",sep = ""))


plot(tail(data[,"CPIAUCSL"],180),type = "l")
lines(forecasts[,1],col = 3)
