#from a differential analysis (diffana)

new_file<-function(data,header=T,nocolnames=F,sep=";",tab="\t",nothing="",dec=",",point="."){
nom<-as.character(substitute(data)) 
data<-read.table(FILE<-file.choose(),na.strings="NA", dec=dec,sep=sep,header=header,fill=T,quote="")
attach(assign(nom,data,pos=1))
setwd(dirname(FILE))
}

#if particular gene
rnaseq<-subset(rnaseq,rnaseq$Tcp1%in%gene)

Tcp1new<-as.vector(rnaseq[,1])
rnaseq<-rnaseq[,-1]
rnaseq<-as.matrix(rnaseq)
rownames(rnaseq)<-Tcp1new

#remove cl19 D2
rnaseq<-rnaseq[,-14]

rnaseqcenter<-scale(rnaseq,scale=F,center=T)
pca<-prcomp(rnaseqcenter)
corpca<-cor(rnaseqcenter,pca$x)

#plot pca
quartz()
plot(pca$rotation[, 1], pca$rotation[, 2],xlab = "PC1", ylab = "PC2",main="mean",col=c(rep("blue",3),rep("red",3)))
text(pca$rotation[, 1], pca$rotation[, 2], labels = rownames(pca$rotation),pos=3,xpd=T)
quartz()
plot(pca$rotation[, 2], pca$rotation[, 3],xlab = "PC2", ylab = "PC3",main="mean",col=c(rep("blue",3),rep("red",3)))
text(pca$rotation[, 2], pca$rotation[, 3], labels = rownames(pca$rotation),pos=3,xpd=T)
quartz()

#barplot %variance
pourcvar <- round(pca$sdev[1:length(pca$sdev)]^2/sum(pca$sdev^2)*100,2)
cumvar<-cumsum(pca$sdev^2)/sum(pca$sdev^2)*100
quartz()
bar<-barplot(pourcvar,col="black",ylab="% of variances",names=paste("PC",1:ncol(pca$rotation),sep=""),ylim=c(0,100),main="",las=2)
points(bar,cumvar,pch=21,col="red",xpd=T)
abline(h=c(0,100),lty=2,col="black",lwd=2)



#plot cor
plot(corpca[,1],corpca[,2],xlim=c(-1,+1),ylim=c(-1,+1),type="n",xlab = "PC1", ylab = "PC2",main="mean",col=c(rep("blue",3),rep("red",3)))
abline(h=0,v=0)
symbols(0,0,circles=1,inches=F,add=T)
text(corpca[,1],corpca[,2],labels=rownames(corpca),cex=0.5,xpd=T,col=c(rep("blue",3),rep("red",3)))

quartz()
plot(corpca[,2],corpca[,3],xlim=c(-1,+1),ylim=c(-1,+1),type="n",xlab = "PC2", ylab = "PC3",main="mean",col=c(rep("blue",3),rep("red",3)))
abline(h=0,v=0)
symbols(0,0,circles=1,inches=F,add=T)
text(corpca[,2],corpca[,3],labels=rownames(corpca),cex=0.5,xpd=T,col=c(rep("blue",3),rep("red",3)))

#if interesting gene to write
gene_interet<-c("Nup210l","Nup133","Magohb","Acta1","Pax6","Zfp677","Rasgrp1","Mxd4","Foxn4","Thsd7a","Lnx1","Lfng","Ezh1","Tmem132c","Pdgfra","Sh3rf3","Cyp26a1","Sp8","Adcy2","Zfp503")
text(corpca[,1],corpca[,2],labels=rownames(corrnaseq),col=ifelse(rownames(corrnaseq)%in%gene_interet,"black","transparent"),cex=0.5,xpd=T)

########supplemental##########
text(corpca[,1],corpca[,2],labels=rownames(corpca),col=ifelse((corpca[,1]<0&corpca[,1]>-0.5&corpca[,2]<0&corpca[,2]>-0.5),"green","transparent"),cex=0.5,xpd=T)

print(rownames(corpca)[(corpca[,1]<0&corpca[,1]>-0.5&corpca[,2]<0&corpca[,2]>-0.5)])

#plot en neg et pos = meme valeur
xlim=c(-max(abs(pca2$rotation[, 1])),+max(abs(pca2$rotation[, 1]))),ylim=c(-max(abs(pca2$rotation[, 2])),+max(abs(pca2$rotation[, 2])))
##################

#plot percentage of variance
pourcvar <- round(pca$sdev[1:length(pca$sdev)]^2/sum(pca$sdev^2)*100,2)
cumvar<-cumsum(pca$sdev^2)/sum(pca$sdev^2)*100
bar<-barplot(pourcvar,col="black",ylab="% of variances",names=paste("PC",1:ncol(pca$x),sep=""),ylim=c(0,100))
points(bar,cumvar,pch=21,col="red",xpd=T)
abline(h=c(0,100),lty=2,col="black",lwd=2)
