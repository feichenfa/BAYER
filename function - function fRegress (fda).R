###
###
### functional response with
###            (concurrent) functional explanatory variable
###
###

##
##  predict knee angle from hip angle;  from demo('gait', package='fda')

##
## formula interface
##
gait <- gait
gaittime  <- as.matrix((1:20)/21)
gaitrange <- c(0,20)
gaitbasis <- create.fourier.basis(gaitrange, nbasis=21)
harmaccelLfd <- vec2Lfd(c(0, (2*pi/20)^2, 0), rangeval=gaitrange)
gaitfd <- smooth.basisPar(gaittime, gait,
                          gaitbasis, Lfdobj=harmaccelLfd, lambda=1e-2)$fd
hipfd  <- gaitfd[,1]
kneefd <- gaitfd[,2]

par(mfrow=c(2,1));plot(hipfd); grid(); title("Hip Angle")
plot(kneefd); grid(); title("Knee Angle")

knee.hip.f <- fRegress(kneefd ~ hipfd)

par(mfrow=c(2,2))
plot(kneefd); grid(); title("Knee Angle")
plot(hipfd); grid(); title("Hip Angle")
#plot(knee.hip.f$yhatfdobj); title("Yhat - Expected Knee Angle")
plot(knee.hip.f$betaestlist$const); title("Beta0"); grid()
plot(knee.hip.f$betaestlist$hipfd); title("Beta1"); grid()

## I switched the response & predictor
hip.knee.f <- fRegress(hipfd ~ kneefd)
