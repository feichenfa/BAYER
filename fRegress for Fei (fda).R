rm(list=ls())
library(fda)
library(tidyverse)

fei_data <- readRDS("fei_data.rds")



# convert to fda
basis <- create.bspline.basis(rangeval = c(0,5001), nbasis = 25)

data_fd <- smooth.basis(argvals = 1:5001,fei_data[,-1] %>% as.matrix(),basis)$fd

par(mfrow=c(1,1))
plot(data_fd);



## fRegress
temp_inlet <- names(fei_data[,-1]) %>% as.numeric()

contRegression <- fRegress(data_fd ~ temp_inlet)

par(mfrow=c(2,2))
plot.fd(contRegression$betaestlist$const$fd); grid(); title(main="Constant Regression Curve")
plot.fd(contRegression$betaestlist$temp_inlet); grid(); title(main = "Tin Variable Coefficient")
plot(contRegression$yhatfdobj$fd); grid(); title("yhat");legend("bottomright", legend = c("345","350","355"), fill = c("black", "red","green"))
