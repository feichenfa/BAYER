


rm(list=ls())

options(max.print=1000000)##show all data in console
options(scipen=999)##remove scientific notation
#source("/mnt/CODE/functional Inputs.Rmd")


r1=readRDS("/mnt/CODE/fRegress_results_func_and_scalar.rds")
r1$betalist$const$fd$fdnames



pred.y= r1$yhatfdobj$y
colnames(pred.y)

ts.plot(pred.y)
colnames(pred.y)
u.Tin_sca =unique(Tin_sca)
u.Tin_sca
u.Tin_sca.l= length(u.Tin_sca)

palette(rainbow(u.Tin_sca.l))
palette()

mycolor=
ifelse ( Tin_sca == 345, 1, 
         ifelse (  Tin_sca == 346, 2, 
                  ifelse ( Tin_sca == 348, 3,
                           ifelse ( Tin_sca == 350, 4,
                                    ifelse ( Tin_sca == 351, 5,
                                             ifelse ( Tin_sca == 353, 6,
                                                      ifelse (Tin_sca == 355,7,
                                                              ifelse (Tin_sca == 356,8,
                                                                      ifelse (Tin_sca == 358,9,
                                                                              ifelse (Tin_sca == 360,10,
                                                                                      ifelse (Tin_sca == 361,11,
                                                                                              ifelse (Tin_sca == 365,12,NA
                                                      ))))))) ) ) ) ) )




plot(0:500,pred.y[,1],type="l",col="red", lwd=3 , ylim=c(0,1.3))
i=2
for (i in 2:dim(pred.y)[2]){
  print(paste("i=",i,mycolor[i],sep=""))
  
  lines(0:500,pred.y[,i], col=mycolor[i], lwd=3)
}
plot(pred.y)


Tc_sca

u.Tc_sca =unique(Tc_sca)
u.Tc_sca
sort(u.Tc_sca)
u.Tc_sca.l= length(u.Tc_sca)
u.Tc_sca.l

palette(rainbow(u.Tc_sca.l))
palette()


mycolor2=
  ifelse ( Tc_sca == 250, 1, 
           ifelse (  Tc_sca == 275, 2, 
                     ifelse ( Tc_sca == 300, 3,
                              ifelse ( Tc_sca == 251, 4,
                                       ifelse ( Tc_sca == 276, 5,
                                                ifelse ( Tc_sca == 301, 6,
                                                         ifelse (Tc_sca == 253,7,
                                                                 ifelse (Tc_sca == 278,8,
                                                                         ifelse (Tc_sca == 303,9,
                                                                                 ifelse (Tc_sca == 256,10,
                                                                                         ifelse (Tc_sca == 281,11,
                                                                                                 ifelse (Tc_sca == 306,12,
                                                                                                         ifelse (Tc_sca == 260,12,
                                                                                                                 ifelse (Tc_sca == 285,12,
                                                                                                                         ifelse (Tc_sca == 310,12,NA
                                                                                                 ))))))) ) ) ) ) ) )))




plot(0:500,pred.y[,1],type="l",col="red", lwd=3 , ylim=c(0,1.3),main="cooling temp")
i=2
for (i in 2:dim(pred.y)[2]){
  print(paste("i=",i,mycolor2[i],sep=""))
  
  lines(0:500,pred.y[,i], col=mycolor2[i], lwd=3)
}





