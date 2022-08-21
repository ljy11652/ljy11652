###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######生信自学网: http://www.biowolf.cn/
######合作邮箱：2749657388@qq.com
######答疑微信: 18520221056

#install.packages("pheatmap")

setwd("C:\\Users\\lexb4\\Desktop\\m6A\\18.ClusterCliHeatmap")      #设置工作目录
rt=read.table("clusterCliExp.txt",sep="\t",header=T,row.names=1,check.names=F)    #读取文件
outpdf="heatmap.pdf"

library(pheatmap)
Type=read.table("clusterCliGroup.Sig.txt",sep="\t",header=T,row.names=1,check.names=F)
Type=Type[order(Type$Cluster),]
rt=rt[,row.names(Type)]

pdf(outpdf,height=6.5,width=10)
pheatmap(rt, annotation=Type, 
         color = colorRampPalette(c("green", "white", "red"))(50),
         cluster_cols =F,
         fontsize=8,
         fontsize_row=8,
         scale="row",
         show_colnames=F,
         fontsize_col=3)
dev.off()


###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######生信自学网: http://www.biowolf.cn/
######合作邮箱：2749657388@qq.com
######答疑微信: 18520221056