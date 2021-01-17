j0<-read.table(FILE<-file.choose(),na.strings="NA", dec=",",sep=";",header=T,fill=T)
attach(j0)

j3<-read.table(FILE<-file.choose(),na.strings="NA", dec=",",sep=";",header=T,fill=T)
attach(j3)

setwd(dirname(FILE))


j3_2<-j3[colnames(j3)%in%colnames(j0)] #ici data.frame pour matrice utiliser la virgule [,xxx] (indifférent pour df) 


#j0<-j0[,order(colnames(j0))]
j3_2=j3[,intersect(colnames(j0),colnames(j3))]


matdiffj0=data.frame(matrix(ncol=252,nrow=1000))
colnames(matdiffj0)<-colnames(j0)

matdiffj3=data.frame(matrix(ncol=252,nrow=1000))
colnames(matdiffj3)<-colnames(j0)

for (i in colnames(j0)){
diff=data.frame(setdiff(as.vector(j0[,i]),as.vector(j3_2[,i])))
diffna<-c(rep(NA,nrow(matdiffj0)-nrow(diff)))
na2mat=data.frame(matrix(diffna,ncol=1))
colnames(na2mat)<-colnames(diff)
mat1=data.frame(rbind(diff,na2mat))
colnames(mat1)<-i
matdiffj0[,i]<-mat1[,i]
}

for (i in colnames(j0)){
diff=data.frame(setdiff(as.vector(j3_2[,i]),as.vector(j0[,i])))
diffna<-c(rep(NA,nrow(matdiffj0)-nrow(diff)))
na2mat=data.frame(matrix(diffna,ncol=1))
colnames(na2mat)<-colnames(diff)
mat1=data.frame(rbind(diff,na2mat))
colnames(mat1)<-i
matdiffj3[,i]<-mat1[,i]
}


matdiffj0=matdiffj0[apply(matdiffj0, 1, function(y) !all(is.na(y))),]
matdiffj3=matdiffj3[apply(matdiffj3, 1, function(y) !all(is.na(y))),]


DIR<-c(dirname(FILE))
DIR2<-c(paste(DIR,"matdiffj0.csv",sep="/"))
write.table(matdiffj0,DIR2,sep=";",row.names=F,na="")

DIR<-c(dirname(FILE))
DIR2<-c(paste(DIR,"matdiffj3.csv",sep="/"))
write.table(matdiffj3,DIR2,sep=";",row.names=F,na="")

rm(list=ls())
