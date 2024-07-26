library(tidyverse)
library(deSolve)


#params
parms <- c(
  Ca = .87725294608097,
  Cin = 1,
  Q=100,
  k0 = 7.2*exp(10),
  Tin = 350,
  V=100,
  rho=100,
  Cp = .239,
  mdelH=5*exp(4),
  EoverR = 8750,
  k0=7.2*exp(10),
  UA = 5*exp(4)
)



#Tin = 350

#reaction rate
rA <- function(t,y,parms){
  with(as.list(c(y, parms)), {
    rA <- k*exp(-EoverR/temp) * Ca
    list(rA)
  })
}
t <- seq(from = 0, to = 500, length.out = 5001)
y <- c(temp=400)
parms_rA <- c(k = 7.2*exp(10), EoverR = 8750, Ca = .87725294608097)
#parms_rA <- c(k = 14*exp(10), EoverR = 8750, Ca = .87725294608097)

out_ra <- ode(y, times=t, func = rA, parms = parms_rA)
rA <- out_ra[,2]
rA <- as.numeric(rA)

### concentration derivative
dCadt <- function(t,y, parms){ # parms =c(q,V,Caf, Ca, rA)
  with(as.list(c(y,parms)),{
    dCadt <- q/V*(Caf - Ca) - rA
    list(dCadt)
  })
}
t <- seq(from = 0, to = 500, length.out = 5001)
#Caf=1
y <- c(Caf=rep(1,5001))
parms_dCa <- c(q= 100, V=100, Ca=rep(.87725294608097,5001), rA=out_ra[,2])

out_dCa <- ode(y, times=t, func = dCadt, parms = parms_dCa) 
  
  
### this function is dependent on the rA Diff Eq at line 29, how do I incorporate?


dTdt <- function(t,y,parms){   ## parms <- c(q,V,Tf,Temp,mdelH, rho,Cp,rA,UA,Tc)
  q/V*(Tf - T) + mdelH/(rho*Cp)*rA + UA/V/rho/Cp*(Tc-Temp)
}




##code
parms=c(
# Feed Concentration (mol/m^3)
Caf = Cin,

# Parameters:
# Volumetric Flowrate (m^3/sec)
q = Q,

# Volume of CSTR (m^3)
V = 100,

# Density of A-B Mixture (kg/m^3)
rho = 1000,

# Heat capacity of A-B Mixture (J/kg-K)
Cp = 0.239,

# Heat of reaction for A->B (J/mol)
mdelH = 5e4,

# E - Activation energy in the Arrhenius Equation (J/mol)
# R - Universal Gas Constant = 8.31451 J/mol-K
EoverR = 8750,

# Pre-exponential factor (1/sec)
k0 = 7.2*exp(10),

# U - Overall Heat Transfer Coefficient (W/m^2-K)
# A - Area - this value is specific for the U calculation (m^2)
UA = 5*exp(4),

# reaction rate
rA = k0*exp(-EoverR/T)*Ca,

# Calculate concentration derivative
dCadt = q/V*(Caf - Ca) - rA
)

t= seq(from = 0, to = 500, length.out = 5001)

Tf= seq(from = 300, to = 305, length.out = 5001)


library(deSolve)
time=t
y= 305
#parms<- c(r=0.1,K=10)
#parms<- c(Caf,q,V,rho,Cp,mdelH,EoverR,k0,UA,rA,dCadt)

model <- function(time, y, parms) {
  with(as.list(c(y,parms)), {
    #dN <- r*N*(1-(N/K))
    #list(dN)
    
    dTdt = q[1]/V*(Tf[1] - T[1]) 
    + mdelH/(rho*Cp)*rA[1] 
    + UA/V/rho/Cp*(Tc[1]-T[1])
    #xdot = np.zeros(2)
    
    list(dTdt)
    
  })
}


out <- ode(y,time, model, parms[1])
plot(out)





