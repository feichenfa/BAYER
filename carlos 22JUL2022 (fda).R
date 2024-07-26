
  
rm(list=ls())
knitr::opts_chunk$set(echo = FALSE)
library(fda)
library(tidyverse)
library(gridExtra)
source("/mnt/fda_CI.R")



### load the data
data <- "/mnt/DATA/Tin & Tc Variation/forReport19Jul2022_4.csv" %>% read_csv() 



data$Replicate <- factor(data$Replicate, levels = unique(data$Replicate))
data$Tin_group <- str_split_fixed(data$Replicate, fixed("_"), n=3)[,1] %>% factor()
data$Tc_group <- str_split_fixed(data$Replicate, fixed("_"),n=3)[,2] %>% factor()

data <- data %>%                             
  group_by(Replicate) %>%
  mutate(time2 = row_number())


data_Tc_sp <- split(data, f=data$Tc_group)


Tcs <- names(data_Tc_sp)


reps <- unique(data$Replicate)
Tin <- substr(reps, 1,3) %>% as.numeric()
Tc <- substr(reps, 5,7) %>% as.numeric()

Tin_Tc <- Tin * Tc


#### Create FDA objects for fRegress
data_con <- data %>%
  select(time, concentrationA, Replicate) %>%
  pivot_wider(names_from = Replicate, values_from = concentrationA)

#### create basis
basis <- create.bspline.basis(rangeval = c(0,150), nbasis = 50)

data_con_fda <- smooth.basis(argvals = 1:150,data_con[1:150,-1] %>% as.matrix(),basis)$fd

plot(data_con_fda)

f_No_Interaction <- fRegress(data_con_fda ~ Tin + Tc +  Tin_Tc)

f_No_Interaction <- fRegress(data_con_fda ~ Tin  )

f_No_Interaction <- fRegress(data_con_fda ~ Tc  )






