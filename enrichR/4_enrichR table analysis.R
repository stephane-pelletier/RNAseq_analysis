#select log neg
data<-read.table(FILE<-file.choose(),na.strings="NA",sep=";",header=T,fill=T)
attach(data)
setwd(dirname(FILE))
data<-c(list.files()[grep(pattern="log-",list.files())])#si la liste des fichier n'est pas presente

#select log pos
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

*************************************************************

all<-c(ls()[grep(pattern=".csv",ls())])

df=data.frame(matrix(nrow=500,ncol=1))
for (j in all){
vec<-as.vector(sort(table(ordered(unlist(get(j)))),decreasing=T)[-1])
#print(vec)
vecname<-names(sort(table(ordered(unlist(get(j)))),decreasing=T)[-1])
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
assign(paste("vec",j,sep="_"),vecmat2)

#assign(paste("vecname",j,sep="_"),vecname)
#df2=data.frame(get(paste("vecname",j,sep="_")),get(paste("vec",j,sep="_")))
#df=cbind(df,df2)
}


all<-c(ls()[grep(pattern="vec_table",ls())])

df=data.frame(matrix(nrow=500,ncol=1))
for (j in all){
repna<-c(rep(NA,500-nrow(get(j))))
vec<-c(rownames(get(j)),repna)
df2=data.frame(vec)
df=cbind(df,df2)
}
df=df[apply(df, 1, function(y) !all(is.na(y))),]
df=df[,-1]

vecdf<-c(unique(as.vector(na.omit(unlist(df)))))

matdf=matrix(ncol=1,nrow=length(vecdf))
rownames(matdf)<-vecdf


for (j in all){
#matdf2=matrix(ncol=1,nrow=length(vecdf))
mattest3=matrix(c(match(rownames(matdf), rownames(get(j)), nomatch = 0)),byrow=F)
colnames(mattest3)<-j
matdf=cbind(matdf,mattest3)
}
matdf=matdf[apply(matdf, 1, function(y) !all(is.na(y))),]
matdf=matdf[,-1]
matdf<-replace(matdf,matdf!=0,1)

matdf2<-apply(matdf,1,sum)
matdf3=data.frame(matdf2[order(matdf2)])
#vec<-as.numeric(rev(as.vector(sort(ordered(unlist(matdf2))))))
#vecname<-names(sort(table(ordered(unlist(matdf2))),decreasing=T)[-1])
matdf4<-as.vector(matdf3[,1])
names(matdf4)<-rownames(matdf3)
matdf4<-rev(matdf4)
matdf4<-data.frame(matdf4)
edit(matdf4)

rm(list=ls())

