```{r}


#使用国内镜像安装包
options("repos"= c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
options(BioC_mirror="http://mirrors.ustc.edu.cn/bioc/")

#安装并加载包
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
```

## 参数设置

```{r}
tumornumber <- 73 #表达矩阵前多少列来自肿瘤样本
normalnumber <- 73 #表达矩阵后多少列来自正常样本
signaturename <- "TGFB" 
# 设置基因分类阈值，用于判断基因的表达状态
halfwidth <- 0.025
# 样本分类个数
ClusterNumber <- 1
#样本类对应颜色
ClusterColor = c("#B31E22","#529B40","#020105","#383D8E")
```

## 读取输入文件

需要两个输入文件：基因表达矩阵和感兴趣的基因通路

- easy_input_expr.csv，文章作者提供的32189*197基因表达矩阵(FPKM)，前147列来自肿瘤样本，后面是50个正常对照的表达谱。
- easy_input_signatures.csv，某一通路基因列表，此处是TGFbeta。

```{r}

expr <- read.csv("easy_input_expr.csv",check.names = F,row.names = 1)
expr[1:3,1:3]
signature <- read.csv("easy_input_signatures.csv")[,1]
signature
```

## 估计GSVA富集分数，后面用于检验富集分数在k个类的差异

```{r, warning=FALSE}
siglist <- list()
siglist[[signaturename]] <- signature
signature.gsva <- gsva(as.matrix(expr[,1:tumornumber]),siglist,method = "gsva")
```

## 根据正常样本signature基因表达将肿瘤样本基因表达状态分成3类：高标达、低表达、不变

```{r, warning=FALSE}
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



# 设置组间对比，排列组合
my_comparisons <- list( c("C1", "C2"), 
                        c("C3", "C4"), 
                        c("C2", "C4"), 
                        c("C1","C3"),
                        c("C1","C4"),
                        c("C3","C2"),
                        c("C1","C5"),
                        c("C2","C5"),
                        c("C3","C5"),
                        c("C4","C5"))

#set color

darkred   <- "#F2042C"
green   <- "#4AE95B"
black  <- "#051206"
blue   <- "#5bc0eb"
purple <- "#8C3DB0"





ggplot(data = tmp,aes(x = cluster, #分组列名
                      y = Enrichscore, #连续变量列名
                      fill = cluster))+ #按分组填充颜色
  scale_fill_manual(values = c(darkred, green, black, blue,purple)) + #用自定义颜色填充
  geom_violin(alpha = 0.4, position = position_dodge(width = .75), 
              size = 0.8, color="black") +
  geom_boxplot(notch = TRUE, outlier.size = -1, 
               color="black", lwd=0.8, alpha = 0.7) +
  geom_point(shape = 21, size=2, # 点的性状和大小
             position = position_jitterdodge(), # 让点散开
             color="black", alpha = 1) +
  theme_classic() + 
  ylab("Enrichment score") +
  xlab("cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        #panel.border = element_rect(colour = "black", fill=NA, size=0.2),
        axis.ticks = element_line(size=0.2, color="black"),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 10)) +
  
  # 如果不要组间比较就注释掉下面这行
  stat_compare_means(comparisons = my_comparisons) + 
  stat_compare_means(method = "kruskal.test", label.y = max(tmp$Enrichscore) + 1)

ggsave("boxViolin_multipleGroups.pdf", width = 6, height = 6)
```

# Session Info

```{r}
sessionInfo()
```