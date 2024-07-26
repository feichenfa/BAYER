rm(list=ls())
library(lsa)
library(fda)
library(HMPTrees)
library(tidyverse)
library(gridExtra)
library(grid)
options(scipen=9999)
options(max.print = 10000000) 
z_trans <- function(r) 0.5*(log(1+r, base = exp(1))-log(1-r, base = exp(1))) 



dataFei <- read.csv("/mnt/DATA/Fei/cstr_functional_ontology_synethic_08242022.csv") 
dat_corr <- dataFei %>%
  select(coolingTemp:Temp) %>%
  cor()

round(dat_corr,3)


dat_corr_v <- dat_corr[upper.tri(dat_corr)] 
round(dat_corr_v,3)



data_corr_z <- z_trans(dat_corr_v)
round(data_corr_z,3)

################################################
################################################
par(mfrow=c(2,1))
x <- data_corr_z   ### x is a graph!
sd <- sqrt(1/(5001-3))
x.mat <- x
for(i in 2:100){
  x.mat <- rbind(x.mat, rnorm(15,x,sd=sd)) ### we are generating a new graph in this line
}                 ### x.mat is a population of graphs sampled from the dist of the first x.mat


head(x.mat)
x.cmd <- cmdscale(dist(x.mat), k=1) %>% as.data.frame()
x.cmd$Rank <- rank(x.cmd)
plot(x.cmd[,1], type = "b", main = "CMD of First 100 graphs with unchanged mean & sd")

for(i in 1:10){
  x.mat <- rbind(x.mat, rnorm(15,x + runif(15,0,i*(.02)),2*sd)) ### we are generating a new graph in this line
}                 ### x.mat is a population of graphs sampled from the dist of the first x.mat

x.cmd <- cmdscale(dist(x.mat), k=1) %>% as.data.frame()
x.cmd$Rank <- rank(x.cmd)

plot(1:nrow(x.cmd), x.cmd$Rank, type = "n")
text(1:nrow(x.cmd), x.cmd$Rank, x.cmd$Rank)

head(x.mat)
x.cmd <- cmdscale(dist(x.mat), k=1) %>% as.data.frame()
x.cmd$Rank <- rank(x.cmd)
plot(x.cmd[,1], type = "b", main = "MDS of Original 100 Graphs + 10 Graphs with sd = 2*sd")
abline(v=100)
#####################################################
#####################################################

x.mat2 <- rbind(x.mat, x.mat+5)
x.cmd2 <- cmdscale(dist(x.mat2), k=1) %>% as.data.frame()
x.cmd2$Rank <- rank(x.cmd2)


s2 <- cosine(x.mat2 %>% t())
s2.cmd <- cmdscale(s2,k=1) %>% as.data.frame()
s2.cmd$Rank <- rank(s2.cmd)
cos.dist = NULL
for(i in 2:dim(xy)[1]) cos.dist = c(cos.dist, cosine(xy[(i-1):i,])[1,2])
plot(cos.dist)


##########################
z <- dataFei
z$coolingTemp <- z$coolingTemp + runif(5001,-10,10)
z$Group <- cut(1:5001,100,labels = FALSE)

z1 <- split(z, f=z$Group)
#i <- z1[[1]]

z_c <- lapply(z1, function(i){
  d2 <- i %>%
    select(coolingTemp:Temp) %>%
    cor()
  d2 <- d2[upper.tri(d2)]
  return(d2)
}) %>%
  bind_rows() %>% t()


View(z_c)


f <- cmdscale(dist(z_c),k=1) %>% as.data.frame()
f$Rank <- rank(f$V1)

qcc::qcc(f$Rank[1:40], type = "xbar.one", newdata = f$Rank[41:100])


View(f)







