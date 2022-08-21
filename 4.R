Packages <- c("pheatmap","gplots")#CRAN的包
for(pk in Packages){
  if(!require(pk,character.only = T,quietly = T)) install.packages(pk)
  suppressMessages(library(pk,character.only = T))
}
if(!require("GSVA",character.only = T,quietly = T)){
  if(!require("BiocManager",character.only = T, quietly = TRUE)) install.packages("BiocManager")
  BiocManager::install("GSVA")#Bioconductor的包
}
suppressMessages(library("GSVA",character.only = T))
Sys.setenv(LANGUAGE = "en") #显示英文报错信息
options(stringsAsFactors = FALSE) #禁止chr转成factor


setwd("E:\\LXR\\cholesterol\\TERNARY")

normalnumber <- 72 #表达矩阵后多少列来自正常样本
tumornumber <- 539 #表达矩阵前多少列来自肿瘤样本

signaturename <- "CHOLESTEROL_BIOSYNTHESIS"
# 设置基因分类阈值，用于判断基因的表达状态
halfwidth <- 0.025
# 样本分类个数
ClusterNumber <- 4
#样本类对应颜色
ClusterColor = c("#B31E22","#529B40","#020105","#383D8E")

# expr <- read.table("protein.txt",check.names = F,row.names = 1,)
rt <- read.table("protein.txt",check.names = F,header = T)
rt=as.matrix(rt)
rownames(rt)=rt[,1]
exp=rt[,2:ncol(rt)]
dimnames=list(rownames(exp),colnames(exp))
expr=matrix(as.numeric(as.matrix(exp)),nrow=nrow(exp),dimnames=dimnames)


expr[1:3,1:3]
signature <- read.csv("easy_input_signatures.csv")[,1]
signature

siglist <- list()
siglist[[signaturename]] <- signature
signature.gsva <- gsva(as.matrix(expr[,1:tumornumber]),siglist,method = "gsva")

# 提取signature基因表达矩阵，筛选样本间有差异的基因
rowids <- intersect(rownames(expr), signature)
logdata <- log2(expr[rowids, ] + 0.5)
tumordata <- logdata[, 1:tumornumber] # 输入数据仅为肿瘤样本
var <- apply(tumordata, 1, sd, na.rm=T) # 计算每个基因的标准差
selvar <- var[var>0] # 只选取在肿瘤样本间存在差异的基因
tumordata <- tumordata[names(selvar), ]
normaldata <- logdata[names(selvar), (tumornumber+1):(tumornumber+normalnumber)] # 正常样本的对应表达

# 根据文献方法提供的标准，判断各个基因相对于正常样本高表达或者低表达的状态

# 首先设置基因分类阈值，计算各个基因在正常样本中高表达或者低表达阈值
halfwidth <- 0.025
normaldown <- apply(normaldata, 1, function(x) quantile(x, probs=halfwidth, na.rm=T) )
normalup <- apply(normaldata, 1, function(x) quantile(x, probs=1-halfwidth, na.rm=T) )

# 根据判断表达状态，得到的状态矩阵用于聚类和绘图
for (k in 1:nrow(tumordata)) {
  rowk <- as.numeric(tumordata[k, ])
  out <- rep(0, times=ncol(tumordata)) # 全部分配0
  out[rowk>normalup[k]] <- 1 # 高表达分配1
  out[rowk<normaldown[k]] <- -1 # 低表达分配-1
  tumordata[k, ] <- out
}

# 输出结果，不用于绘图
outdata <- tumordata
outdata[outdata==1] <- "UP" # 高表达标记为UP
outdata[outdata==-1] <- "DOWN" # 低表达标记为DOWN
outdata[outdata==0] <- "NOCHANGE" #不变标记为NOCHANGE
write.csv(outdata,"Ternary.csv",row.names = T,col.names = NA)


### 行和列使用ward.D聚类
hcg <- hclust(dist(tumordata), "ward.D")#行聚类
hcs <- hclust(dist(t(tumordata)), "ward.D")#列聚类
k <- ClusterNumber # 列聚类数目
group <- paste0("C",cutree(hcs,k))
names(group) <- colnames(tumordata)

# 注释信息
annCol <- data.frame(Cluster=group,score=signature.gsva[signaturename,])
# 检验TGFBscore在k个类的差异(非参),计算pvalue
p <- kruskal.test(annCol$score ~ as.factor(annCol$Cluster))$p.value
colnames(annCol)[2] <- paste0(signaturename,"_score P = ",format(p,digits = 3)) # 科学计数法保留2位

# 对应注释信息的颜色
annColors <- list(Cluster = c(C1 = ClusterColor[1],
                              C2 = ClusterColor[2],
                              C3 = ClusterColor[3],
                              C4 = ClusterColor[4]),
                  bluered(64))
names(annColors)[2] <- colnames(annCol)[2]

pct <- paste0(round(rowSums(tumordata == 1)/ncol(tumordata),2) * 100,"%") # 计算上调基因百分比
rownames(tumordata) <- paste0(rownames(tumordata)," (",pct,")") # 行名加上这个百分比

# 绘图
pheatmap(tumordata,
         cluster_rows = hcg,
         cluster_cols = hcs,
         color = c("#283285","#DEDEDE","#651430"),
         annotation_col = annCol,
         annotation_colors = annColors,
         fontsize_col = 2, # 行名大小
         fontsize_row = 8, # 列名大小（也可以不显示）
         fontsize = 6, # 总体大小，目的是调低图例的大小
         cutree_cols = k, # 切割图片
         legend_breaks = c(-1,0,1), # 修改图例的显示位置
         legend_labels = c("mRNA\ndown-regulation","mRNA\nno-change","mRNA\nup-regulation"), # 修改图例各位置的名称
         filename = "Heatmap_TernaryCluster.pdf",width = 7,height = 5)
write.table(annCol, file = "Cluster_score_info.xls", quote = FALSE, sep = "\t", append = FALSE, col.names = TRUE, row.names = TRUE)
```
