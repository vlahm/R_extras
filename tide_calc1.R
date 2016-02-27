x<-seq(0,24,0.0001)

hours<-11
minutes<-26
AMPM<-'AM'
time<-ifelse(AMPM=='PM', (hours*60)+(12*60)+minutes, (hours*60)+minutes)

amp<-12
per<-(2*pi)/12 #[2pi/12 = b] - wave repeats b times in 2pi units along the x
freq<-1

y<-amp*sin(per*x+freq)
plot(x,y,type='l', xlab=c(0,24), ylab=c(-12,12))
# abline(h=0, v=0, lty=2)
# amp*sin(1*x+1)[1]
# 24/(2*pi)
# (2*pi)/12

options("scipen"=100)
which(y==min(y))
length(y)
