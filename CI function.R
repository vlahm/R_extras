n<-50
x<-seq(10,36,length.out=n)
y<-rnorm(n,50,4)
chili<-lm(y~x)
plot(x,y)

CI<-function(model, level=.95, xname, xbounds)
{
  confx<-seq(xbounds[1],xbounds[2], length.out=1000)
  int.vals<-predict(model,newdata=data.frame(xname=confx),interval="confidence", 
                    level=level)
  lines(confx,int.vals[,2], lty="dashed")
  lines(confx,int.vals[,3], lty="dashed")
}

CI(chili,,"x",c(8,37))
