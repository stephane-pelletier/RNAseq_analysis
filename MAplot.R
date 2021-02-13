
new_file<-function(data,header=T,nocolnames=F,sep=";",tab="\t",nothing="",dec=",",point="."){
  nom<-as.character(substitute(data)) 
  data<-read.table(FILE<-file.choose(),na.strings="NA", dec=dec,sep=sep,header=header,fill=T,quote="")
  attach(assign(nom,data,pos=1))
  setwd(dirname(FILE))
}


new_file(Day0_All_ko.wt,sep="\t",dec=".")
new_file(Day3_All_ko.wt,sep="\t",dec=".")
new_file(Day0_ko1.2_wt1.2,sep="\t",dec=".")
new_file(Day3_ko1.2_wt1.2,sep="\t",dec=".")
new_file(Day0_ko1.3_wt1.3,sep="\t",dec=".")
new_file(Day2_ko1.3_wt1.3,sep="\t",dec=".")
new_file(Day3_ko1.3_wt1.3,sep="\t",dec=".")



all<-c(ls()[grep("Day",ls())])

for (i in all){
  x11()
  plot(log2((get(i)$baseMean)),(get(i)$log2FoldChange_ko.wt),pch=".",main=i,cex.main=2.5,col=ifelse((get(i)$padj)<0.05,"red","black"),xlab="log2(baseMean)",ylab="Log2FoldChange_ko.wt",cex.lab=1.4)
  abline(h=-0.75)
  abline(h=0.75)
  abline(v=log2(150))
  text(10,((70*par()$yaxp[1])/100),labels=length((get(i)$baseMean)[(log2((get(i)$baseMean))>log2(150))&((get(i)$log2FoldChange_ko.wt)<(-0.75))&((get(i)$padj)<0.05)]),cex=2.5)
  text(10,((70*par()$yaxp[2])/100),labels=length((get(i)$baseMean)[(log2((get(i)$baseMean))>log2(150))&((get(i)$log2FoldChange_ko.wt)>(0.75))&((get(i)$padj)<0.05)]),cex=2.5)
  dev.print(tiff,paste(i,".","tiff",sep=""),width=400)
}



#several colors for several value

for (i in all){
  x11()
  plot(log2((get(i)$baseMean)),(get(i)$log2FoldChange_ko.wt),pch=".",main=i,cex.main=2.5,col=ifelse((get(i)$padj<0.05&get(i)$padj>0.01),"red",ifelse((get(i)$padj<0.01&get(i)$padj>0.001),"blue",ifelse((get(i)$padj<0.001&get(i)$padj>0.0005),"green",ifelse(get(i)$padj<0.0005,"purple","black")))),xlab="log2(baseMean)",ylab="Log2FoldChange_ko.wt",cex.lab=1.4)
  abline(h=-0.6)
  abline(h=0.6)
  abline(v=log2(150))
  text(10,((70*par()$yaxp[1])/100),labels=length((get(i)$baseMean)[(log2((get(i)$baseMean))>log2(150))&((get(i)$log2FoldChange_ko.wt)<(-0.6))&((get(i)$padj)<0.05)]),cex=2.5)
  text(10,((70*par()$yaxp[2])/100),labels=length((get(i)$baseMean)[(log2((get(i)$baseMean))>log2(150))&((get(i)$log2FoldChange_ko.wt)>(0.6))&((get(i)$padj)<0.05)]),cex=2.5)
}
