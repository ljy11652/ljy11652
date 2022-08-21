###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######������ѧ��: http://www.biowolf.cn/
######�������䣺2749657388@qq.com
######����΢��: 18520221056

#install.packages("vioplot")

library(vioplot)                                                    #���ð�
setwd("C:\\Users\\15216\\Desktop\\m6A\\08.vioplot")                 #���ù���Ŀ¼
normal=72                                                           #������Ʒ��Ŀ
tumor=539                                                           #������Ʒ��Ŀ

rt=read.table("m6Aexp.txt",sep="\t",header=T,row.names=1,check.names=F)   #��ȡ�����ļ�
rt=t(rt)

pdf("vioplot.pdf",height=6,width=14)              #����ͼƬ���ļ�����
par(las=1,mar=c(4,5,3,3))
x=c(1:ncol(rt))
y=c(1:ncol(rt))
plot(x,y,
     xlim=c(0,51),ylim=c(min(rt),max(rt)*1.1),
     main="",xlab="", ylab="Gene expression",
     pch=21,
     cex.lab=1.5,
     col="white",
     xaxt="n")

#��ÿ��m6A��ػ���ѭ��������vioplot����������ɫ��ʾ�������ú�ɫ��ʾ
for(i in 1:ncol(rt)){
  normalData=rt[1:normal,i]
  tumorData=rt[(normal+1):(normal+tumor),i]
  vioplot(normalData,at=3*(i-1),lty=1,add = T,col = 'blue')
  vioplot(tumorData,at=3*(i-1)+1,lty=1,add = T,col = 'red')
  wilcoxTest=wilcox.test(normalData,tumorData)
  p=round(wilcoxTest$p.value,3)
  mx=max(c(normalData,tumorData))
  lines(c(x=3*(i-1)+0.2,x=3*(i-1)+0.8),c(mx,mx))
  text(x=3*(i-1)+0.5, y=mx*1.05, labels=ifelse(p<0.001, paste0("p<0.001"), paste0("p=",p)), cex = 0.8)
}
text(seq(1,53,3),-5,xpd = NA,labels=colnames(rt),cex = 1,srt = 45,pos=2)
dev.off()

###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######������ѧ��: http://www.biowolf.cn/
######�������䣺2749657388@qq.com
######����΢��: 18520221056
