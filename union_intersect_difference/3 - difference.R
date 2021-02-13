
nb=2
name=c("d0","d3")

for (i in name){
  data<-read.table(FILE<-file.choose(),na.strings="NA", dec=",",sep=";",header=T,fill=T)
  attach(data)
  newname<-get(as.character(substitute(i)))
  attach(assign(newname,data,pos=1))
  DIR<-dirname(FILE)
}




setwd(dirname(DIR))





d3_2<-d3[colnames(d3)%in%colnames(d0)] #ici data.frame pour matrice utiliser la virgule [,xxx] (indifférent pour df) 


#d0<-d0[,order(colnames(d0))]
d3_2=d3[,intersect(colnames(d0),colnames(d3))]


d0=d0[,apply(d0, 2, function(y) !all(is.na(y)))]
d3_2=d3_2[,apply(d3_2, 2, function(y) !all(is.na(y)))]



matdiffd0=data.frame(matrix(ncol=252,nrow=1000))
colnames(matdiffd0)<-colnames(d0)

matdiffd3=data.frame(matrix(ncol=252,nrow=1000))
colnames(matdiffd3)<-colnames(d0)

for (i in colnames(d0)){
  diff=data.frame(setdiff(as.vector(d0[,i]),as.vector(d3_2[,i])))
  diffna<-c(rep(NA,nrow(matdiffd0)-nrow(diff)))
  na2mat=data.frame(matrix(diffna,ncol=1))
  colnames(na2mat)<-colnames(diff)
  mat1=data.frame(rbind(diff,na2mat))
  colnames(mat1)<-i
  matdiffd0[,i]<-mat1[,i]
}

for (i in colnames(d0)){
  diff=data.frame(setdiff(as.vector(d3_2[,i]),as.vector(d0[,i])))
  diffna<-c(rep(NA,nrow(matdiffd0)-nrow(diff)))
  na2mat=data.frame(matrix(diffna,ncol=1))
  colnames(na2mat)<-colnames(diff)
  mat1=data.frame(rbind(diff,na2mat))
  colnames(mat1)<-i
  matdiffd3[,i]<-mat1[,i]
}


matdiffd0=matdiffd0[apply(matdiffd0, 1, function(y) !all(is.na(y))),]
matdiffd3=matdiffd3[apply(matdiffd3, 1, function(y) !all(is.na(y))),]


#DIR<-c(dirname(FILE))
DIR2<-c(paste(DIR,"matdiffd0.csv",sep="/"))
write.table(matdiffd0,DIR2,sep=";",row.names=F,na="")

#DIR<-c(dirname(FILE))
DIR2<-c(paste(DIR,"matdiffd3.csv",sep="/"))
write.table(matdiffd3,DIR2,sep=";",row.names=F,na="")

rm(list=ls())
