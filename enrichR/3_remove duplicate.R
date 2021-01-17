data<-read.table(FILE<-file.choose(),na.strings="NA",sep=";",header=T,fill=T)
attach(data)


df=data.frame(matrix(ncol=1,nrow=54))
for (i in colnames(data)){
v1<-as.character(get(i))
v1<-unique(v1)
repna=rep(NA,nrow(df)-length(v1))
v1<-c(v1,repna)
df=cbind(df,v1)
}
edit(df)
rm(list=ls())

#or

df2<-apply(df,2,unique)
maxdf2<-max(unlist(lapply(df2, lapply,function(x) max(sapply(df2, length)))))
nadf2<- lapply(df2,function(v) { c(v, rep(NA, maxdf2-length(v)))})
df2_new<-data.frame(nadf2)

length(df2_new$a[!is.na(df2_new$a)])
#######
#try without loop

apply(df,2,unique)

a<-c("a","b","a","c","f")
b<-c("a","b","b","c","f")
d<-c("a","b","c","d","e")

df<-data.frame(a,b)
df<-data.frame(a,d,b)

df2<-apply(df,2,unique)
#lapply(df2,length)



maxdf2<-max(unlist(lapply(df2, lapply,function(x) max(sapply(x, length)))))
nadf2<- lapply(df2,lapply,lapply,function(v) { c(v, rep(NA, maxdf2-length(v)))})
df22<-data.frame(nadf2)
