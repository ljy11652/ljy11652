###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######������ѧ��: http://www.biowolf.cn/
######�������䣺2749657388@qq.com
######����΢��: 18520221056

#
install.packages("pheatmap")

setwd("C:\\Users\\DELL\\Desktop\\07.sigPeatmap")      #���ù���Ŀ¼
rt=read.table("geneSigExp.txt",sep="\t",header=T,row.names=1,check.names=F)
rt=log2(rt+1)

library(pheatmap)
Type=c(rep("N",72),rep("T",539))    #�޸Ķ��պʹ�������Ʒ��Ŀ
names(Type)=colnames(rt)
Type=as.data.frame(Type)

bk <- c(seq(-5.5,-0.1,by=0.01),seq(0,5.5,by=0.01))
pdf("heatmap.pdf",height=5,width=10)
pheatmap(rt, 
         annotation=Type, 
         #color = colorRampPalette(c("blue", "white", "red"))(50),
         color = c(colorRampPalette(colors = c("blue","white"))(length(bk)/2),colorRampPalette(colors = c("white","red"))(length(bk)/2)),
         breaks=bk,
         legend_breaks=seq(-8,8,2),
         cluster_cols =F,
         show_colnames = F,
         scale="row",
         fontsize = 10,
         fontsize_row=10,
         fontsize_col=3)
dev.off()

###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######������ѧ��: http://www.biowolf.cn/
######�������䣺2749657388@qq.com
######����΢��: 18520221056