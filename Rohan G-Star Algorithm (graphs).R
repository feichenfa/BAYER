library(HMPTrees)
library(data.table)
library(tidyverse)
library(akima)
options(scipen=9999)
#source("IoMT Functions.R")


data <- fread("/mnt/DATA/Fei/cstr_functional_ontology_synethic_08242022.csv")

data$coolingTemp <- data$coolingTemp + rnorm(5001,0,10)

data <- data %>% filter(Mode == "Abnormal")

timeSplit4 <- function(data, secs = 7200, rows = 604800) {
  data <- data[1:rows,]
  #data <- data %>% select(sensors)
  data$TimeGroup <- cut(1:dim(data)[1], seq(0, dim(data)[1], secs), labels = FALSE)
  #data$SPO2 <- data$SPO2 + runif(dim(data)[1], -.1, .1)
  data.split <- split(data, f = data$TimeGroup)
  return(data.split)
}


data_split <- timeSplit4(data, secs = 50, rows = 500)
data_split <- lapply(data_split, function(i){
  i %>% 
    dplyr::select(coolingTemp, concentrationA_inlet, Temp_inlet, flowRate, concentrationA, Temp)
})

#tri <- function(x) x[upper.tri(x)]

data_cor <- lapply(data_split, cor)



new_cors <- lapply(data_cor, function(i){      ### i is each element of data_cor
  cordf <- as.data.frame(as.table(i)) 
  combinations = combn( colnames(i) , 2 , FUN = function(x) { paste(x, collapse = "_" )})
  cordf = cordf[ cordf$Var1 != cordf$Var2 , ]
  cordf = cordf[ paste( cordf$Var1 , cordf$Var2 , sep = "_" ) %in% combinations , ]
  cordf$Vars <- paste(cordf$Var1,cordf$Var2, sep = "_:_")
  cordf <- cordf %>% select(Vars, Freq)
  #####
  #cordf <- t(cordf) %>% as.data.frame() 
  #colnames(cordf) <- cordf[1,]
  #cordf <- cordf[-1,]  
  #####
}) %>% 
  imap(.x = ., ~ set_names(.x, c("Vars", .y))) %>% 
  reduce(dplyr::inner_join, by = "Vars") %>%
  column_to_rownames(var = "Vars")


#y <- lapply(data_cor, function(x) x[upper.tri(x)])

#z <- do.call("rbind",y)

#colnames(z) <- c("cA_In:Tc","Tin:Tc","Tin:cA_In", "etc....")

MLE.abnorm <- getMLEandLoglike(new_cors)
round(MLE.abnorm$mleTree,2)

round(cbind(MLE$mleTree, MLE.norm$mleTree, MLE.abnorm$mleTree),2)

lapply(data_split, function(i){
  apply(i,2,var) 
}) 





