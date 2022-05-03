
# must install FRED package
#library(devtools)
#install_github("cykbennie/fbi")

library(fbi)
library(tidyverse)
library(TTR)

start_date = "1960-01-01"

data = fredmd("https://files.stlouisfed.org/files/htdocs/fred-md/monthly/current.csv")
data_raw = fredmd("https://files.stlouisfed.org/files/htdocs/fred-md/monthly/current.csv",
                  transform = FALSE)
varlist = fredmd_description
vars = intersect(colnames(data),varlist$fred)

data = data %>% as.tibble()%>%
  select(all_of(c("date",vars)))
varlist = varlist%>%filter(fred%in%vars)
prices_varlist = varlist%>%filter(group=="Prices",tcode==6)

data = data%>% as_tibble()%>%
  select( -all_of(prices_varlist$fred) )

prices = data_raw %>% as_tibble() %>%
  select(all_of(prices_varlist$fred))%>%
  mutate_all(.funs = function(x)100*c(NA,x%>%log()%>%diff()))

data = cbind(data%>%as.data.frame(),prices%>%as.data.frame())

data = data %>%
  filter(date>=start_date)%>%
  select_if(~ !any(is.na(.)))

save(data,file = "data/data.rda")

