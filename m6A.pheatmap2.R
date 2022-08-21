###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######������ѧ��: http://www.biowolf.cn/
######�������䣺2749657388@qq.com
######����΢��: 18520221056

#install.packages("pheatmap")

setwd("C:\\Users\\15216\\Desktop\\m6A\\26.RiskClinicalHeatmap")      #���ù���Ŀ¼
rt=read.table("riskCliExp.txt",sep="\t",header=T,row.names=1,check.names=F)    #��ȡ�ļ�
rt=t(rt)
outpdf="heatmap.pdf"

library(pheatmap)
Type=read.table("riskCliGroup.sig.txt",sep="\t",header=T,row.names=1,check.names=F)
Type=Type[order(Type$Risk),]
rt=rt[,row.names(Type)]

pdf(outpdf,height=6.3,width=10)
pheatmap(rt, annotation=Type, 
         color = colorRampPalette(c("green", "white", "red"))(50),
         cluster_cols =F,
         fontsize=7.5,
         fontsize_row=8,
         scale="row",
         show_colnames=F,
         fontsize_col=3)
dev.off()


###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######������ѧ��: http://www.biowolf.cn/
######�������䣺2749657388@qq.com
######����΢��: 18520221056
