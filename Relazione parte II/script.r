tab=read.csv2("tabella.csv", row.names = 1) 
str(tab)
head(tab)
tab.st=data.frame(scale(tab))
plot(tab.st,lower.panel=NULL,pch=20)
round(cor(tab.st),2)

library(cluster)
library(MASS)

#---kmeans--- 
as=rep(0,15)
for(k in 2:15){
  cl=kmeans(tab.st,k,nstart=25)$cluster
  as[k]=mean(silhouette(cl,dist(tab.st))[,3])
}
plot(2:15,as[2:15],type="b",pch=20)

layout(matrix(1:4,2,2))
plot(silhouette(kmeans(tab.st,2,nstart=25)$cluster,dist(tab.st)))
plot(silhouette(kmeans(tab.st,3,nstart=25)$cluster,dist(tab.st)))
plot(silhouette(kmeans(tab.st,4,nstart=25)$cluster,dist(tab.st)))
plot(silhouette(kmeans(tab.st,5,nstart=25)$cluster,dist(tab.st)))
layout(1)

layout(t(1:2))
tab.km=kmeans(tab.st,2,nstart=25)
tab.pca=princomp(tab.st)
plot(tab.pca$scores, col=1+tab.km$cluster, pch=20)
points(predict(tab.pca,tab.km$centers),col="black",pch=19)
text(tab.pca$scores,labels=as.character(row.names(tab)),col=1+tab.km$cluster,pos=3,cex=0.6)

tab.km=kmeans(tab.st,3,nstart=25)
tab.pca=princomp(tab.st)
plot(tab.pca$scores, col=1+tab.km$cluster, pch=20)
points(predict(tab.pca,tab.km$centers),col="black",pch=19)
text(tab.pca$scores,labels=as.character(row.names(tab)),col=1+tab.km$cluster,pos=3,cex=0.6)
layout(1)

#---PAM---
c=rep(0,15)
for(i in 2:15){
  c[i]=pam(tab.st,i)$silinfo$avg.width
}
plot(2:15,c[2:15],type="b",pch=19) 

k=2
layout(t(1:4))
plot(pam(tab.st,k))

k=3
plot(pam(tab.st,k))
layout(1) 

#---PAM con distanza manhattan---
c=rep(0,15)
for(i in 2:15){
  c[i]=pam(tab.st,i,metric="manhattan")$silinfo$avg.width
}
plot(2:15,c[2:15],type="b",pch=19) 

k=2
layout(t(1:2))
plot(pam(tab.st,k,metric="manhattan"))

k=3
plot(pam(tab.st,k,metric="manhattan"))

k=4
plot(pam(tab.st,k,metric="manhattan"))
layout(1)

k=2
tab.pca=princomp(tab.st)
plot(tab.pca$scores,pch=19,col=1+pam(tab.st,k,metric="manhattan")$cluster)
text(tab.pca$scores,labels=as.character(row.names(tab)),col=1+pam(tab.st,k,metric="manhattan")$cluster,pos=3,cex=0.8)

summary(pam(tab.st,k,metric="manhattan"))
#---Metodi Gerarchici---
d<-dist(tab.st) 

layout(t(1:3))
tab.hcc=hclust(d,"complete") 
plot(tab.hcc,hang=-1,cex=0.6)
tab.hca=hclust(d,"average")
plot(tab.hca,hang=-1,cex=0.6)
tab.hcs=hclust(d,"single")
plot(tab.hcs,hang=-1,cex=0.6)
layout(1)

table(cutree(tab.hcs,10))
table(cutree(tab.hcs,6))

layout(t(1:2))
as=rep(0,15)
for(i in 2:15){
  tab.cut=cutree(tab.hca,i)
  as[i]=mean(silhouette(tab.cut,d)[,3])
}
plot(2:15,as[2:15],type="b")

as=rep(0,15)
for(i in 2:15){
  tab.cut=cutree(tab.hcc,i)
  as[i]=mean(silhouette(tab.cut,d)[,3])
}
plot(2:15,as[2:15],type="b")
layout(1)

layout(matrix(1:4,2,2))
tab.cut=cutree(tab.hcc,2)
plot(silhouette(tab.cut,d),col=heat.colors(2),border=par("fg"))

tab.cut=cutree(tab.hcc,3)
plot(silhouette(tab.cut,d),col=heat.colors(3),border=par("fg"))

tab.cut=cutree(tab.hca,2)
plot(silhouette(tab.cut,d),col=heat.colors(2),border=par("fg"))

tab.cut=cutree(tab.hca,3)
plot(silhouette(tab.cut,d),col=heat.colors(3),border=par("fg"))
layout(1)

tab.pca=princomp(tab.st)
plot(tab.pca$scores,col=1+cutree(tab.hcc,2),pch=20)
text(tab.pca$scores,labels=as.character(row.names(tab)),col=1+cutree(tab.hcc,2),pos=3,cex=0.6)

d<-dist(tab.st, method = "manhattan") 

layout(t(1:3))
tab.hcc=hclust(d,"complete") 
plot(tab.hcc,hang=-1,cex=0.6)
tab.hca=hclust(d,"average")
plot(tab.hca,hang=-1,cex=0.6)
tab.hcs=hclust(d,"single")
plot(tab.hcs,hang=-1,cex=0.6)
layout(1)

table(cutree(tab.hcs,10))
table(cutree(tab.hcs,6))

layout(t(1:2))
as=rep(0,15)
for(i in 2:15){
  tab.cut=cutree(tab.hca,i)
  as[i]=mean(silhouette(tab.cut,d)[,3])
}
plot(2:15,as[2:15],type="b")

as=rep(0,15)
for(i in 2:15){
  tab.cut=cutree(tab.hcc,i)
  as[i]=mean(silhouette(tab.cut,d)[,3])
}
plot(2:15,as[2:15],type="b")
layout(1)

layout(matrix(1:4,2,2))
tab.cut=cutree(tab.hcc,2)
plot(silhouette(tab.cut,d),col=heat.colors(2),border=par("fg"))

tab.cut=cutree(tab.hcc,3)
plot(silhouette(tab.cut,d),col=heat.colors(3),border=par("fg"))

tab.cut=cutree(tab.hca,2)
plot(silhouette(tab.cut,d),col=heat.colors(2),border=par("fg"))

tab.cut=cutree(tab.hca,3)
plot(silhouette(tab.cut,d),col=heat.colors(3),border=par("fg"))
layout(1)

tab.pca=princomp(tab.st)
plot(tab.pca$scores,col=1+cutree(tab.hcc,2),pch=20)
text(tab.pca$scores,labels=as.character(row.names(tab)),col=1+cutree(tab.hcc,2),pos=3,cex=0.6)
