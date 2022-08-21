###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######生信自学网: http://www.biowolf.cn/
######合作邮箱：2749657388@qq.com
######答疑微信: 18520221056

#install.packages("survival")

clusterNum=4  #聚类分为几类

col_1 = 'red'
col_2 = 'green'
col_3 = 'black'
col_4 = 'blue'

setwd("C:\\Users\\15216\\Desktop\\TGF\\TERNARY")    #工作目录（需修改）
library(survival)
rt=read.table("clusterTime.txt",header=T,sep="\t",check.names=F)
rt$futime=rt$futime/365                                     #如果以月为单位，除以30；以年为单位，除以365

diff=survdiff(Surv(futime, fustat) ~cluster,data = rt)
pValue=1-pchisq(diff$chisq,df=clusterNum-1)
if(pValue<0.001){
  pValue=signif(pValue,4)
  pValue=format(pValue, scientific = TRUE)
}else{
  pValue=round(pValue,3)
}

fit <- survfit(Surv(futime, fustat) ~ cluster, data = rt)
summary(fit)

pdf(file="survival.pdf",width = 5.5,height =5)
plot(fit, 
     lwd=2,
     # col=rainbow(clusterNum),
     col=c(col_1,col_2,col_3,col_4),
     xlab="Time (year)",
     mark.time=T,
     ylab="Survival rate",
     main=paste("Survival curve (p=", pValue ,")",sep=""))
legend("topright", 
     paste0("cluster",1:clusterNum), 
     lwd=2, 
     # col=rainbow(clusterNum),
     col=c(col_1,col_2,col_3,col_4))

# p <- fit[["conf.int"]]
# 
# # 添加相关性
# text(7.5,0,adj = 0,bquote(~italic(P)~" = "~.(pValue)),cex = 1.0)

dev.off()

###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######生信自学网: http://www.biowolf.cn/
######合作邮箱：2749657388@qq.com
######答疑微信: 18520221056