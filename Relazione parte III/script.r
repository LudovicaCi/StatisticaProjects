tab=read.csv2("tabella.csv", header=F)
head(tab)
data = tab[,2]
head(data)
tp=ts(data,frequency=12,start=c(2004,1))
#Analisi della struttura della serie storica
ts.plot(tp)
acf(tp)
acf(tp,60)
plot(diff(tp))
acf(diff(tp))
acf(diff(tp),60)
m_data=matrix(data,12,17)
par(bg="black")
ts.plot(m_data,col=heat.colors(17))
ts.plot(scale(m_data,scale=F),col=heat.colors(17))
par(bg="white")
data.d=decompose(tp)
plot(data.d)
layout(t(1:2))
plot(data.d$random)
lines(data.d$seasonal,col="red")
plot(data.d$trend,col="blue",ylim=c(-400,10000))
lines(data.d$random)
lines(data.d$seasonal,col="red")
layout(1)

#Holt-Winters(metodo di smorzamento esponenziale con trend)
tp.set=HoltWinters(tp,gamma = F)
plot(tp.set)
tp.set$alpha
tp.set$beta
for(alpha in 1:9){
  for(beta in 1:9){
    plot(HoltWinters(tp,alpha=alpha/10,beta=beta/10,gamma=F),xlab=paste("alpha=",alpha/10," - beta",beta/10))
  }
}

x=1:24
coefficients(lm(tp[1:24]~x))
tp.set=HoltWinters(tp,alpha = 0.2,beta = tp.set$beta,gamma=F,l.start=8768.5,b.start=3.378)
plot(tp.set)

#Autovalidazione metodo esponenziale con trend 
train=window(tp,end=c(2019,12))
test=window(tp,2020)
tp_t = HoltWinters(train,alpha = 0.2,beta = tp.set$beta,gamma=F,l.start=8768.5,b.start=3.378)
tp_p=predict(tp_t,12)
sqrt(mean((tp_p-test)^2))
tp_p 
test

#Analisi dei residui 
tp.set.r=residuals(tp.set)
1-var(tp.set.r)/var(window(tp,c(2005,1)))
plot(tp.set.r, type="p", pch=20)
hist(tp.set.r,20,freq = F)
lines(sort(tp.set.r),dnorm(sort(tp.set.r),mean(tp.set.r),sd(tp.set.r)),col="red")
qqnorm(tp.set.r, pch=20)
qqline(tp.set.r)
shapiro.test(tp.set.r)

#Predizione futura 
plot(tp.set,predict(tp.set,6),main="Previsione a 6 mesi")
lines(predict(tp.set,6)+quantile(tp.set.r,0.05),col="blue")
lines(predict(tp.set,6)+quantile(tp.set.r,0.95),col="blue")
predict(tp.set,6)

#Autoregressione con il metodo dei minimi quadrati 
acf(tp)
pacf(tp)
tp.ls = ar(tp,method="ols")
tp.ls$order 
1-tp.ls$var/var(tp[20:204])
o = tp.ls$order
a = tp.ls$ar
b = tp.ls$x.intercept
tp.ls.an = tp
for (i in (o + 1):length(tp)) {
  tp.ls.an[i] = sum(rev(a) * tp[(i - o):(i - 1)]) + mean(tp) * (1 - sum(a)) +
    b
}
plot(tp)
lines(tp.ls.an, col = "blue", lwd = 2)

#analisi dei residui 
tp.ls.r = as.double(na.omit(tp.ls$resid))
tp.ls.fitted = as.double(na.omit(tp - tp.ls$resid))
plot(tp.ls.r, pch = 20)
var(tp.ls.r)/var(tp.ls.r + tp.ls.fitted)
acf(tp.ls.r)
pacf(tp.ls.r)
hist(tp.ls.r, 20, freq = F)
lines(density(tp.ls.r), col = "red")
lines(sort(tp.ls.r), dnorm(sort(tp.ls.r), mean(tp.ls.r), sd(tp.ls.r)), col = "blue")
qqnorm(tp.ls.r, pch = 20)
qqline(tp.ls.r)
shapiro.test(tp.ls.r)

#Autovalidazione 
train=window(tp,end=c(2019,12))
test=window(tp,2020)
tp_t = ar(train,method="ols")
tp_p=predict(tp_t,n.ahead=12,se.fit=FALSE)
sqrt(mean((tp_p-test)^2))
tp_p 
test
