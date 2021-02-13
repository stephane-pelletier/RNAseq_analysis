
call_list=1
colname_if_list="V2"

pattern="Log"
column_names_gene="V18"


##########################################################

value01<-c(0,1)
stopifnot(call_list%in%value01)

newname<-get(as.character(substitute(colname_if_list)))
newname2<-get(as.character(substitute(column_names_gene)))

if (call_list==1){
  data<-read.table(FILE<-file.choose(),na.strings="NA", dec=",",sep="",header=F)
  attach(data)
  data<-c(data[,newname][grep(pattern=pattern,data[,newname])])
  setwd(dirname(FILE))
}else{
  FILE<-dirname(file.choose())
  setwd(FILE)
  data<-c(list.files()[grep(pattern=pattern,list.files())])
}

  

for (i in data){
    assign(gsub("-","__",i,fixed=T),read.table(i,na.strings="NA",dec=".",sep="\t",header=F,fill=T,quote=""))
  }
  
  all<-c(ls()[grep(pattern=".txt",ls())])
  all=all[-1]
  
  for (i in all){
   # print(nrow(get(i)))
  }
  
  for (i in all){
  #  print(ncol(get(i)))
  }
  
  mat=data.frame(matrix(ncol=96,nrow=98))
  colnames(mat)<-all
  
  for (i in all){
    mat1=data.frame(get(i)[colnames(get(i))==newname2])
    colnames(mat1)<-i
    na2mat<-c(rep(NA,nrow(mat)-nrow(mat1)))
    na2mat=data.frame(matrix(na2mat,ncol=1))
    colnames(na2mat)<-i
    mat1=data.frame(rbind(as.vector(mat1),as.vector(na2mat)))
    mat[,i]<-mat1[,i]
  }

  if (call_list==1){  
    DIR<-c(dirname(FILE))
  }else{
    DIR<-FILE
  }
  DIR2<-c(paste(DIR,"mat.csv",sep="/"))
  write.table(mat,DIR2,sep=";",row.names=F,na="")
  
  
  rm(list=ls())
  