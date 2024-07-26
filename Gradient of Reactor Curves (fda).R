### making a gradient for Fei


## setup
rm(list=ls())
knitr::opts_chunk$set(echo = TRUE)
library(fda)
library(tidyverse)
library(gridExtra)
library(grid)
source("/mnt/fda_CI.R")
options(scipen=9999)
options(max.print = 10000000) 


dataAll <- "/mnt/DATA/Tin & Tc Variation/Tc200to500Tin200to500_by5.csv" %>% read.csv()
dataAll$Replicate <- factor(dataAll$Replicate, levels = unique(dataAll$Replicate))
dataAll <- dataAll %>%
  group_by(Replicate) %>%
  mutate(time2=row_number(),
         Tin_group=plyr::round_any(Temp_inlet,10),
         Tc_group=plyr::round_any(coolingTemp,10))

seq <- seq(1,3721,length.out = 60) %>% round(0)
reps <- unique(dataAll$Replicate)
reps <- reps[seq]

d <- dataAll %>% filter(Replicate %in% reps)

d %>%
  filter(time2<=150) %>%
  #filter(Replicate %in% d$Replicate) %>%
  ggplot(aes(x=time2, y=concentrationA, color=Replicate, group=Replicate)) + geom_line() + theme(legend.position="none") #+ scale_y_continuous(trans="log10")




### load the raw data
dataFei <- "/mnt/DATA/Tin & Tc Variation/Tc250to300_Tin300to350_Unif_dist_16Aug2022.csv" %>% read.csv()
dataFei$Replicate <- factor(dataFei$Replicate, levels = unique(dataFei$Replicate))
dataFei <- dataFei %>%
  group_by(Replicate) %>%
  mutate(time2=row_number(),
         Tin_group=plyr::round_any(Temp_inlet,10),
         Tc_group=plyr::round_any(coolingTemp,10))

dataFei %>%
  filter(time2<=150) %>%
  #filter(Replicate %in% d$Replicate) %>%
  ggplot(aes(x=time2, y=concentrationA, color=Replicate, group=Replicate)) + geom_line() + theme(legend.position="none") #+ scale_y_continuous(trans="log10")



### make fd object
data_con <- dataFei %>%
  select(time2, concentrationA, Replicate) %>%
  pivot_wider(names_from = Replicate, values_from = concentrationA)

basis <- create.bspline.basis(rangeval = c(0,150), nbasis = 50)

data_con_fda <- smooth.basis(argvals = 1:150,data_con[1:150,-1] %>% as.matrix(),basis)$fd

plot(data_con_fda)

### setup regression
Tin <- dataFei %>%
  group_by(Replicate) %>%
  distinct(Temp_inlet)
Tin <- Tin$Temp_inlet %>% as.vector() #%>% as.factor()

Tc <- dataFei %>%
  group_by(Replicate) %>%
  distinct(coolingTemp) 
Tc <- Tc$coolingTemp %>% as.vector()
Tc2 <- (Tc-mean(Tc))/sd(Tc)
Tin2 <- (Tin-mean(Tin))/sd(Tin)
#Tin2 <- sample(Tin2)
Tin_Tc <- Tin2 * Tc2



## regression
fReg <- fRegress(data_con_fda ~ Tin2 + Tc2 + Tin_Tc)
plot(fReg$yhatfdobj)

par(mfrow=c(2,2))
plot(fReg$betaestlist$const)
plot(fReg$betaestlist$Tin2)
plot(fReg$betaestlist$Tc2)
plot(fReg$betaestlist$Tin_Tc)


### store coefficients as time-series matrix
coefs.df <- data.frame(time = 1:150,
                       B0 = eval.fd(1:150,fReg$betaestlist$const$fd),
                       B1 = eval.fd(1:150,fReg$betaestlist$Tin$fd)
) %>%
  mutate(Tin_300 = B0 + B1*300,
         Tin_250 = B0+B1*250,
           Tin_200 = B0+B1*200,
         Tin_400 = B0+B1*400)

lines(1:150, coefs.df$Tin_400)

par(mfrow=c(1,1))
plot(coefs.df$time, coefs.df$Tin_300, ylim=c(0,1), type = "l");grid()
abline(h=0.2)
#lines(coefs.df$time, coefs.df$Tin_250, col = "red")
#lines(coefs.df$time, coefs.df$Tin_200, col = "red")

temps <- seq(300,3000,by=300)
for(i in temps){
  x <- coefs.df$B0 + coefs.df$B1*i
  lines(coefs.df$time, x, col = "red")
}

x <- coefs.df$B0 + coefs.df$B1*3000
lines(coefs.df$time, x, col = "blue")

