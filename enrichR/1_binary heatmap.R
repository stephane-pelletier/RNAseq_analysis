data<-read.table(FILE<-file.choose(),na.strings="NA", dec=".",sep="\t",header=T,fill=T)
attach(data)
setwd(dirname(FILE))
data<-c(list.files()[grep(pattern="log",list.files())])#si la liste des fichier n'est pas presente



for (i in data){
assign(gsub("-","__",i,fixed=T),read.table(i,na.strings="NA",dec=".",sep="\t",header=T,fill=T,quote=""))
}

all<-c(ls()[grep(pattern=".txt",ls())])

for (j in all){
mat=data.frame(matrix(nrow=1000,ncol=1))
for (i in 1:nrow(get(j))){
l1<-length(unlist(strsplit(as.vector(get(j)[i,"Genes"]),";")))
l2<-unlist(strsplit(as.vector(get(j)[i,"Genes"]),";"))
if(get(j)[i,"P.value"]<0.05){
if(length(l2)>=1){
l22<-l2
l3<-as.vector(get(j)[i,"Term"])
l22na<-c(rep(NA,1000-length(l22)))
l22<-c(l22,l22na)
mat1=data.frame(l22)
colnames(mat1)<-l3
mat=cbind(mat,mat1)
}
else{}
}
else{}
}
mat=mat[apply(mat, 1, function(y) !all(is.na(y))),]
mat=mat[,-1]
colnames(mat)<-gsub("-","_",colnames(mat),fixed=T)
matunlist<-as.vector(unlist(mat)[!is.na(unlist(mat))])
names(matunlist)<-names(unlist(mat)[!is.na(unlist(mat))])
matfin=matrix(nrow=length(unique(matunlist)),ncol=1)
rownames(matfin)<-unique(matunlist)
for (i in mat){
mattest2=matrix(nrow=length(unique(matunlist)),ncol=1)
rownames(mattest2)<-unique(matunlist)
mattest3=matrix(c(match(rownames(matfin), i, nomatch = 0)),byrow=F)
rownames(mattest3)<-unique(matunlist)
matfin=cbind(matfin,mattest3)
}
matfin=matfin[,-1]
colnames(matfin)<-colnames(mat)
matfin<-replace(matfin,matfin!=0,1)
matfin<-matfin[order(rownames(matfin)),]
matfin<-matfin[,order(colnames(matfin))]
j<-gsub(".txt","",j,fixed=T)
j<-gsub("__","-",j,fixed=T)
tablemat<-c(paste("table_",j,".csv",sep=""))
tablematbin<-c(paste("binary_table_",j,".csv",sep=""))
DIR<-dirname(FILE)
titre0<-c(paste("enrichR",format(Sys.time(), "_%a%d%b%Y"),sep=""))
DIR0<-c(paste(DIR,titre0,sep="/"))
dir.create(DIR0)
titre1<-c(paste(DIR0,tablemat,sep="/"))
titre2<-c(paste(DIR0,tablematbin,sep="/"))
write.table(mat[,order(colnames(mat))],titre1,sep=";",row.names=F,na="",quote=F)
#write.table(cbind(c(rownames(matfin),matfin)),titre2,sep=";",row.names=F,na="")
j2<-c(paste(j,"   ","(",nrow(matfin),")",sep=""))
quartz()
heatmap(matfin,col=c("white","black"),Rowv=NA,Colv=NA,margins=c(20,5),cex.Row=0.3,scale="none",main=j2)
}

rm(list=ls())
