######Video source: https://ke.biowolf.cn
######������ѧ��: https://www.biowolf.cn/
######΢�Ź��ںţ�biowolf_cn
######�������䣺2749657388@qq.com
######����΢��: 18520221056

#install.packages("corrplot")

library(corrplot)                                              #���ð�
setwd("C:\\Users\\lexb4\\Desktop\\mRNAsi\\17.corrplot")        #���ù���Ŀ¼
rt=read.table("keyGeneExp.txt",sep="\t",header=T,row.names=1,check.names=F)    #��ȡ�����ļ�

#ɾ��������Ʒ
group=sapply(strsplit(colnames(rt),"\\-"),"[",4)
group=sapply(strsplit(group,""),"[",1)
group=gsub("2","1",group)
rt=rt[,group==0]

#���������ͼ��
pdf("corrplot.pdf",height=10,width=10)              #����ͼƬ���ļ�����
par(oma=c(0.5,1,1,1.2))
M=cor(t(rt))
corrplot(M, order = "AOE", type = "upper", tl.pos = "lt")
corrplot(M, add = TRUE, type = "lower", method = "number", order = "AOE",
         col = "black", diag = FALSE, tl.pos = "n", cl.pos = "n")
dev.off()

######Video source: https://ke.biowolf.cn
######������ѧ��: https://www.biowolf.cn/
######΢�Ź��ںţ�biowolf_cn
######�������䣺2749657388@qq.com
######����΢��: 18520221056