###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######生信自学网: http://www.biowolf.cn/
######合作邮箱：2749657388@qq.com
######答疑微信: 18520221056

setwd("C:\\Users\\lexb4\\Desktop\\m6A\\17.ClusterGroupSig")
field="Cluster"
flag1="cluster1"
flag2="cluster2"

rt=read.table("clusterCliGroup.txt",sep="\t",header=T,check.names=F)
trainFlag=rt[rt[,field]==flag1,]
trainFlag=cbind(trainFlag,flag="Group1")
testFlag=rt[rt[,field]==flag2,]
testFlag=cbind(testFlag,flag="Group2")
newTable=rbind(trainFlag,testFlag)

newLabels=c("id")
for(i in 2:(ncol(rt)-1) ){
  nameStat=colnames(newTable)[i]
  tableStat=table(newTable[,c(nameStat,"flag")])
  pStat=chisq.test(tableStat)
  pvalue=pStat$p.value
  if(pvalue<0.001){
	  newLabels=c(newLabels,paste0(colnames(newTable)[i],"***"))
	}else if(pvalue<0.01){
	  newLabels=c(newLabels,paste0(colnames(newTable)[i],"**"))
	}else if(pvalue<0.05){
	  newLabels=c(newLabels,paste0(colnames(newTable)[i],"*"))
	}else{
	  newLabels=c(newLabels,colnames(newTable)[i])
	}
	print(paste(colnames(newTable)[i],pvalue,sep=" "))
}
newLabels=c(newLabels,colnames(newTable)[ncol(rt)])
colnames(rt)=newLabels
write.table(rt,file="clusterCliGroup.Sig.txt",sep="\t",row.names=F,quote=F)

###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######生信自学网: http://www.biowolf.cn/
######合作邮箱：2749657388@qq.com
######答疑微信: 18520221056