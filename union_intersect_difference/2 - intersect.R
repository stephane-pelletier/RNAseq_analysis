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

#d0<-d0[!is.na(d0)]
#d3_2<-d3_2[!is.na(d3_2),]

d0=d0[,apply(d0, 2, function(y) !all(is.na(y)))]
d3_2=d3_2[,apply(d3_2, 2, function(y) !all(is.na(y)))]

#d0<-d0[,order(colnames(d0))]
#d3_2=d3[,intersect(colnames(d0),colnames(d3))]




matinter=data.frame(matrix(ncol=252,nrow=1000))
colnames(matinter)<-colnames(d0)

for (i in colnames(d0)){
  inter=data.frame(intersect(as.vector(d0[,i]),as.vector(d3_2[,i])))
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
