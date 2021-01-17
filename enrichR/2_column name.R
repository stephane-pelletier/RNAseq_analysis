namecol<-function(dframe,pattern){
coord<-which(dframe==pattern,arr.ind=TRUE)
name<-colnames(dframe)[seq_along(colnames(dframe))%in%coord[,2]]
print(name)
}


#select log neg
pat<-c("ZFP677","WFIKKN1","THSD7A","RASGRP1","NUP133","MAGOHB","LNX1","LFNG","FOXN4","EZH1","DOC2A","CNTD1","ATP1A2","ARAP3","ACTA1")
data<-read.table(FILE<-file.choose(),na.strings="NA",sep=";",header=T,fill=T)
attach(data)
setwd(dirname(FILE))
data<-c(list.files()[grep(pattern="log-",list.files())])#si la liste des fichier n'est pas presente

#select log pos
pat<-c("ZFP503","TMEM132C","SP8","SH3RF3","PORCN","PDGFRA","NUP210L","MXD4","CYP26A1","ADCY2")
data<-read.table(FILE<-file.choose(),na.strings="NA",sep=";",header=T,fill=T)
attach(data)
setwd(dirname(FILE))
data<-c(gsub("log-","neg",list.files()))#si la liste des fichier n'est pas presente
data<-c(data[grep(pattern="log",data)])#si la liste des fichier n'est pas presente


rm(list=grep("table",ls(),value=T))
for (i in data){
assign(gsub("-","__"
,i,fixed=T),read.table(i,na.strings="NA",sep=";",header=T,fill=T,quote=""))
assign(gsub(",","."
,gsub("-","__"
,i,fixed=T),fixed=T),get(gsub("-","__"
,i,fixed=T)))
}
rm(list=grep(",",ls(),value=T))

all<-c(ls()[grep(pattern=".csv",ls())])

df0=data.frame(matrix(ncol=1,nrow=1000))
for (j in pat){
df0=data.frame(matrix(ncol=1,nrow=1000))
for (i in all){
df1<-c(namecol(get(i),j))
repna<-c(rep(NA,1000-length(df1)))
df1<-c(df1,repna)
df1=data.frame(df1)
df0=cbind(df0,df1)
}
df0=df0[apply(df0, 1, function(y) !all(is.na(y))),]
df0=df0[,-1]
assign(j,df0)
}


for (j in pat){
#vec<-as.vector(sort(table(ordered(unlist(get(j)))),decreasing=T)[-1])
vec<-as.vector(sort(table(ordered(unlist(get(j)))),decreasing=T))
#print(vec)
#vecname<-names(sort(table(ordered(unlist(get(j)))),decreasing=T)[-1])
vecname<-names(sort(table(ordered(unlist(get(j)))),decreasing=T))
#print(vecname)
repna<-c(rep(NA,500-length(vec)))
#print(repna)
#print(length(vec))
vec<-c(vec,repna)
#print(vec)
vecname<-c(vecname,repna)
#print(vecname)
vecmat=matrix(vec,ncol=1)
rownames(vecmat)<-vecname
#print(vecmat)
vecmat2=vecmat[apply(vecmat, 1, function(y) !all(is.na(y))),]
vecmat2=matrix(vecmat2,ncol=1)
rownames(vecmat2)<-as.vector(na.omit(vecname))
colnames(vecmat2)<-j
#assign(paste("vec",j,sep="_"),vecmat2)
assign(paste("vec",j,sep="_"),vecmat2)
}


for (i in pat){
v1=paste("vec",i,sep="_")
edit(get(v1))
}





for (j in pat){
assign(paste("df",j,sep="_"),data.frame(matrix(ncol=1,nrow=1)))
v1=paste("vec",j,sep="_")
test<-strsplit(as.character(rownames(get(v1))),"_")
#test[[1]][seq_along(test[[1]])==1]

for (i in 1:length(test)){
v2<-test[[i]][seq_along(test[[i]])==1]
v2<-data.frame(v2)
assign(paste("df",j,sep="_"),cbind(get(paste("df",j,sep="_")),v2))
}
assign("df",get(paste("df",j,sep="_")))
df=data.frame(df)
df=t(df)
df=df[apply(df, 1, function(y) !all(is.na(y))),]
assign(paste("df",j,sep="_"),data.frame(df))
}

for (i in pat){
v1=paste("df",i,sep="_")
edit(get(v1))
}


rm(list=ls())
