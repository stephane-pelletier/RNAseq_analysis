data<-read.table(FILE<-file.choose(),na.strings="NA", dec=",",sep="",header=F) #choose list
attach(data)

#############
data<-c(list.files()[grep(pattern="Log",list.files())]) #if there is no list
#############

setwd(dirname(FILE))


for (i in data$V2){
assign(gsub("-","__",i,fixed=T),read.table(i,na.strings="NA",dec=".",sep="\t",header=F,fill=T,quote=""))
}

all<-c(ls()[grep(pattern=".txt",ls())])
all=all[-1]

for (i in all){
print(nrow(get(i)))
}

for (i in all){
print(ncol(get(i)))
}

mat=data.frame(matrix(ncol=96,nrow=98))
colnames(mat)<-all

for (i in all){
mat1=data.frame(get(i)[colnames(get(i))=="V18"])
colnames(mat1)<-i
na2mat<-c(rep(NA,nrow(mat)-nrow(mat1)))
na2mat=data.frame(matrix(na2mat,ncol=1))
colnames(na2mat)<-i
mat1=data.frame(rbind(as.vector(mat1),as.vector(na2mat)))
mat[,i]<-mat1[,i]
}

DIR<-c(dirname(FILE))
DIR2<-c(paste(DIR,"mat.csv",sep="/"))
write.table(mat,DIR2,sep=";",row.names=F,na="")

rm(list=ls())
