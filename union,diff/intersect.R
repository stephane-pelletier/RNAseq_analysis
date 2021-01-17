j0<-read.table(FILE<-file.choose(),na.strings="NA", dec=",",sep=";",header=T,fill=T)
attach(j0)

j3<-read.table(FILE<-file.choose(),na.strings="NA", dec=",",sep=";",header=T,fill=T)
attach(j3)

setwd(dirname(FILE))


j3_2<-j3[colnames(j3)%in%colnames(j0)] #here data.frame for matrix used comma  [,xxx] (indifferent for df) 


#j0<-j0[,order(colnames(j0))]
j3_2=j3[,intersect(colnames(j0),colnames(j3))]


matinter=data.frame(matrix(ncol=252,nrow=1000))
colnames(matinter)<-colnames(j0)

for (i in colnames(j0)){
inter=data.frame(intersect(as.vector(j0[,i]),as.vector(j3_2[,i])))
interna<-c(rep(NA,nrow(matinter)-nrow(inter)))
na2mat=data.frame(matrix(interna,ncol=1))
colnames(na2mat)<-colnames(inter)
mat1=data.frame(rbind(inter,na2mat))
colnames(mat1)<-i
matinter[,i]<-mat1[,i]
}

matinter=matinter[apply(matinter, 1, function(y) !all(is.na(y))),]



DIR<-c(dirname(FILE))
DIR2<-c(paste(DIR,"matinter.csv",sep="/"))
write.table(matinter,DIR2,sep=";",row.names=F,na="")

rm(list=ls())
