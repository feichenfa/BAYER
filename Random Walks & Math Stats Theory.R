# stabilie prop = .5
# random walk
#

par(mfrow=c(2,1))
x1 = sample(c(0,1), 10001, replace=T, prob=c(0.5, .5))
y1 = cumsum(x)
xy1 = y / 1:length(y)
plot(xy1, type='l')
grid()
abline(h=.5, col='red')


#x2 = sample(c(0,1), 10001, replace=T, prob=c(0.5, .5))
x2 <- x1
probs = seq(.5, 1, by=0.0001)
a = NULL
for(i in probs) a = c(a, sample(c(0,1), 1, replace=T,
                                prob=c(1-i, i)))

x2=c(x,a)
y2 = cumsum(x)
xy2 = y / 1:length(y)
plot(xy2, type='l')
grid()
abline(h=.5, col='red')