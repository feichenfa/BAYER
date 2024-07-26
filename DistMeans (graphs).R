
library(HMPTrees)
library(data.table)
library(tidyverse)
library(akima)
library(qcc)
options(scipen=9999)


timeSplit4 <- function(data, secs = 7200, rows = 604800) {
  data <- data[1:rows,]
  #data <- data %>% select(sensors)
  data$TimeGroup <- cut(1:dim(data)[1], seq(0, dim(data)[1], secs), labels = FALSE)
  #data$SPO2 <- data$SPO2 + runif(dim(data)[1], -.1, .1)
  data.split <- split(data, f = data$TimeGroup)
  return(data.split)
}


#data <- fread("/mnt/DATA/Fei/cstr_functional_ontology_synethic_08242022.csv")
data <- fread("/Users/fchen1/OneDrive - Bayer/Codes/Rscripts/BAYER/DATA/Fei/cstr_functional_ontology_synethic_08242022.csv")

#for(s in c(5,10,25,50)){
s=10
  new_cors <- data %>%
    timeSplit4(secs = s, rows = 5000) %>%
    lapply(function(i) {
      i %>% 
        dplyr::select(concentrationA_inlet, Temp_inlet, flowRate, concentrationA, Temp)
    }) %>%
    lapply(cor) %>%
    lapply(function(i){
      cordf <- as.data.frame(as.table(i)) 
      combinations = combn( colnames(i) , 2 , FUN = function(x) { paste(x, collapse = "_" )})
      cordf = cordf[ cordf$Var1 != cordf$Var2 , ]
      cordf = cordf[ paste( cordf$Var1 , cordf$Var2 , sep = "_" ) %in% combinations , ]
      cordf$Vars <- paste(cordf$Var1,cordf$Var2, sep = "_:_")
      cordf <- cordf %>% select(Vars, Freq)
    }) %>%
    imap(.x = ., ~ set_names(.x, c("Vars", .y))) %>% 
    reduce(dplyr::inner_join, by = "Vars") %>%
    column_to_rownames(var = "Vars")
  
  X <- t(new_cors) %>% as.data.frame()
  
  gmean <- apply(X[c(1,2),],2,function(x) mean(x, na.rm = TRUE))
  gdist <- dist(rbind(gmean,X[3,]))
  gmean <- X[1,]
  gdist <- 0
  for(i in 2:nrow(X)){
    gdist[i] <- dist(rbind(gmean,X[i,]), method = "euc")
    z <- (gmean-X[i,])^2
    gdist[i] <- gdist[i]/sum(!is.na(z))
    gmean <- apply(X[1:i,],2,function(x) mean(x, na.rm = TRUE))
    
  }
  qcc::qcc(gdist[!is.na(gdist)],type = "xbar.one", title = paste("Reactor Data (",s," timepoint chunks)", sep = ""))
#}





gdist
hist(gdist)
qcc(gdist[!is.na(gdist)],type = "xbar.one", main = "Unhealthy Baby N276")
plot(gdist[!is.na(gdist)],type = "b")





# DistMeanSPC <- list(d=vector(mode = "numeric"),m=data.frame(matrix(NA, nrow=100,ncol=10)))
# 
# DistMeanSPC$d[1] <- dist(X[c(1:2),])
# DistMeanSPC$m[1,] <- apply(X[c(1,2),],2, function(i) mean(i,na.rm=TRUE))
# colnames(DistMeanSPC$m) <- colnames(X)
# for(i in 2:(nrow(X)-1)){
#   DistMeanSPC$d[i] <- dist(rbind(DistMeanSPC$m[i-1,],X[i+1,]))  
#   DistMeanSPC$m[i,] <- apply(X[1:(i+1),],2, function(x) mean(x, na.rm=TRUE))
# }
