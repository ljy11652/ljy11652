###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######生信自学网: http://www.biowolf.cn/
######合作邮箱：2749657388@qq.com
######答疑微信: 18520221056

#install.packages("vioplot")

library(vioplot)                                                    #引用包
setwd("C:\\Users\\15216\\Desktop\\m6A\\08.vioplot")                 #设置工作目录
normal=72                                                           #正常样品数目
tumor=539                                                           #肿瘤样品数目

rt=read.table("m6Aexp.txt",sep="\t",header=T,row.names=1,check.names=F)   #读取输入文件
rt=t(rt)

pdf("vioplot.pdf",height=6,width=14)              #保存图片的文件名称
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

#对每个m6A相关基因循环，绘制vioplot，正常用蓝色表示，肿瘤用红色表示
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
######生信自学网: http://www.biowolf.cn/
######合作邮箱：2749657388@qq.com
######答疑微信: 18520221056

