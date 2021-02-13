

new_file<-function(data,header=T,nocolnames=F,sep=";",tab="\t",nothing="",dec=",",point="."){
  nom<-as.character(substitute(data)) 
  data<-read.table(FILE<-file.choose(),na.strings="NA", dec=dec,sep=sep,header=header,fill=T,quote="")
  attach(assign(nom,data,pos=1))
  setwd(dirname(FILE))
}

new_file(rnaseq)
new_file(gene2plot)
gene<-as.vector(unlist(gene2plot))
gene<-unique(gene[gene!=""])

rnaseq<-subset(rnaseq,rnaseq$Tcp1%in%gene)
rnaseq2<-rnaseq[,-1]

rnaseq22<-as.matrix(rnaseq2)
matwt<-rnaseq22[,1:3]
matko<-rnaseq22[,4:6]

mat2<-matko/matwt
logmat2<-log2(mat2)
rownames(logmat2)<-rnaseq$Tcp1
colnames(logmat2)<-c("D0","D2","D3")
logmat2<-logmat2[order(rownames(logmat2)),]
colfunc<-colorRampPalette(c("blue","white","red"))
colfunc<-colorRampPalette(c("blue","red"))
colors2 <- (colfunc(length(logmat2)))
colors2 <- colors2[sort(unique(rank(logmat2)))]
legend_image <- as.raster(matrix(rev(colfunc(20)), ncol=1))

x11()
heatmap(logmat2,col=colors2,scale="none",Rowv=NA,Colv=NA,margins=c(5,15),breaks=seq(min(logmat2[logmat2!=-Inf]),max(logmat2[logmat2!=Inf]),length.out=length(colors2)+1))
rasterImage(legend_image, 0.85,0.25,0.9,0.75)
text(x=0.95, y = c(0.27,mean(c(0.27,0.73)),0.73), labels = c(signif(min(logmat2[logmat2!=-Inf]),1),signif(mean(c(min(logmat2[logmat2!=-Inf]),max(logmat2[logmat2!=Inf]))),1),signif(max(logmat2[logmat2!=Inf]),1)),xpd=T,adj=0)

x11()
heatmap(logmat2,col=colors2,scale="none",Rowv=NA,Colv=NA,margins=c(5,15),breaks=seq(-10,10,length.out=length(colors2)+1))
rasterImage(legend_image, 0.85,0.25,0.9,0.75)
text(x=0.95, y = c(0.27,mean(c(0.27,0.73)),0.73), labels = c(-10,0,10),xpd=T,adj=0)
