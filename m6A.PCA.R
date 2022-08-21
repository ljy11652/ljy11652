###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######生信自学网: http://www.biowolf.cn/
######合作邮箱：2749657388@qq.com
######答疑微信: 18520221056

#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("limma")

#install.packages("ggplot2")

#pca analysis
library(limma)
setwd("C:\\Users\\15216\\Desktop\\m6A\\11.PCA")                                              #设置工作目录
rt=read.table("symbol.txt",sep="\t",header=T,check.names=F)                             #读取输入文件
rt=as.matrix(rt)
rownames(rt)=rt[,1]
exp=rt[,2:ncol(rt)]
dimnames=list(rownames(exp),colnames(exp))
data=matrix(as.numeric(as.matrix(exp)),nrow=nrow(exp),dimnames=dimnames)
data=avereps(data)
data=data[rowMeans(data)>0.5,]

type=sapply(strsplit(colnames(data),"\\-"),"[",4)
type=sapply(strsplit(type,""),"[",1)
type=gsub("2","1",type)
data=t(data[,type==0])

data.class <- rownames(data)
data.pca <- prcomp(data, scale. = TRUE)                                  #PCA分析
write.table(predict(data.pca),file="newTab.xls",quote=F,sep="\t")        #输出新表

#可视化
library(ggplot2)
cluster=read.table("cluster.txt",sep="\t",header=F)                      #读取分型文件
group=paste0("cluster",as.vector(cluster[,2]))
pcaPredict=predict(data.pca)
PCA = data.frame(PCA1 = pcaPredict[,1], PCA2 = pcaPredict[,2],group=group)

pdf(file="PCA.pdf",height=5,width=6.5)             #保存输入出文件
ggplot(data = PCA, aes(PCA1, PCA2)) + geom_point(aes(color = group)) +
    theme_bw()+
    theme(plot.margin=unit(rep(1.5,4),'lines'))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()

###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######生信自学网: http://www.biowolf.cn/
######合作邮箱：2749657388@qq.com
######答疑微信: 18520221056