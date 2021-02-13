
data=1
Gene_name="Tcp1"

gene=0
scaling=F
center=T

nom<-get(as.character(substitute(Gene_name)))
if (data==1){
  data<-read.csv(FILE<-file.choose(),na.strings="NA", dec=",",sep=";",header=T)
  attach(data)
  DIR<-dirname(FILE)
}else{options(warn=-1)
}

data<-data[,-1]
data<-as.matrix(data)
rownames(data)<-get(nom)

rnaseq<-data
rm(data)


#if particular gene
#rnaseq<-subset(rnaseq,rnaseq$Tcp1%in%gene)

if (gene!=0){
  rnaseq<-subset(rnaseq,rnaseq$nom%in%gene)
  
  Tcp1new<-as.vector(rnaseq[,1])
  rnaseq<-rnaseq[,-1]
  rnaseq<-as.matrix(rnaseq)
  rownames(rnaseq)<-Tcp1new
}else{}

#rnaseq<-rnaseq[,1]
#rownames(rnaseq)<-get(nom)

rnaseqcenter<-scale(rnaseq,scale=scaling,center=center)

pca<-prcomp(rnaseqcenter)
corpca<-cor(rnaseqcenter,pca$x)

#plot pca
x11()
plot(pca$rotation[, 1], pca$rotation[, 2],xlab = "PC1", ylab = "PC2",main="mean",col=c(rep("blue",3),rep("red",3)))
text(pca$rotation[, 1], pca$rotation[, 2], labels = rownames(pca$rotation),pos=3,xpd=T)
x11()
plot(pca$rotation[, 2], pca$rotation[, 3],xlab = "PC2", ylab = "PC3",main="mean",col=c(rep("blue",3),rep("red",3)))
text(pca$rotation[, 2], pca$rotation[, 3], labels = rownames(pca$rotation),pos=3,xpd=T)


#barplot %variance
pourcvar <- round(pca$sdev[1:length(pca$sdev)]^2/sum(pca$sdev^2)*100,2)
cumvar<-cumsum(pca$sdev^2)/sum(pca$sdev^2)*100
x11()
bar<-barplot(pourcvar,col="black",ylab="% of variances",names=paste("PC",1:ncol(pca$rotation),sep=""),ylim=c(0,100),main="",las=2)
points(bar,cumvar,pch=21,col="red",xpd=T)
abline(h=c(0,100),lty=2,col="black",lwd=2)



#plot cor
x11()
plot(corpca[,1],corpca[,2],xlim=c(-1,+1),ylim=c(-1,+1),type="n",xlab = "PC1", ylab = "PC2",main="mean",col=c(rep("blue",3),rep("red",3)))
abline(h=0,v=0)
symbols(0,0,circles=1,inches=F,add=T)
text(corpca[,1],corpca[,2],labels=rownames(corpca),cex=0.5,xpd=T,col=c(rep("blue",3),rep("red",3)))

x11()
plot(corpca[,2],corpca[,3],xlim=c(-1,+1),ylim=c(-1,+1),type="n",xlab = "PC2", ylab = "PC3",main="mean",col=c(rep("blue",3),rep("red",3)))
abline(h=0,v=0)
symbols(0,0,circles=1,inches=F,add=T)
text(corpca[,2],corpca[,3],labels=rownames(corpca),cex=0.5,xpd=T,col=c(rep("blue",3),rep("red",3)))




