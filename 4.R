Packages <- c("pheatmap","gplots")#CRAN�İ�
for(pk in Packages){
  if(!require(pk,character.only = T,quietly = T)) install.packages(pk)
  suppressMessages(library(pk,character.only = T))
}
if(!require("GSVA",character.only = T,quietly = T)){
  if(!require("BiocManager",character.only = T, quietly = TRUE)) install.packages("BiocManager")
  BiocManager::install("GSVA")#Bioconductor�İ�
}
suppressMessages(library("GSVA",character.only = T))
Sys.setenv(LANGUAGE = "en") #��ʾӢ�ı�����Ϣ
options(stringsAsFactors = FALSE) #��ֹchrת��factor


setwd("E:\\LXR\\cholesterol\\TERNARY")

normalnumber <- 72 #�������������������������
tumornumber <- 539 #�������ǰ������������������

signaturename <- "CHOLESTEROL_BIOSYNTHESIS"
# ���û��������ֵ�������жϻ���ı���״̬
halfwidth <- 0.025
# �����������
ClusterNumber <- 4
#�������Ӧ��ɫ
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

# ��ȡsignature����������ɸѡ�������в���Ļ���
rowids <- intersect(rownames(expr), signature)
logdata <- log2(expr[rowids, ] + 0.5)
tumordata <- logdata[, 1:tumornumber] # �������ݽ�Ϊ��������
var <- apply(tumordata, 1, sd, na.rm=T) # ����ÿ������ı�׼��
selvar <- var[var>0] # ֻѡȡ��������������ڲ���Ļ���
tumordata <- tumordata[names(selvar), ]
normaldata <- logdata[names(selvar), (tumornumber+1):(tumornumber+normalnumber)] # ���������Ķ�Ӧ����

# �������׷����ṩ�ı�׼���жϸ���������������������߱�����ߵͱ����״̬

# �������û��������ֵ������������������������и߱�����ߵͱ�����ֵ
halfwidth <- 0.025
normaldown <- apply(normaldata, 1, function(x) quantile(x, probs=halfwidth, na.rm=T) )
normalup <- apply(normaldata, 1, function(x) quantile(x, probs=1-halfwidth, na.rm=T) )

# �����жϱ���״̬���õ���״̬�������ھ���ͻ�ͼ
for (k in 1:nrow(tumordata)) {
  rowk <- as.numeric(tumordata[k, ])
  out <- rep(0, times=ncol(tumordata)) # ȫ������0
  out[rowk>normalup[k]] <- 1 # �߱������1
  out[rowk<normaldown[k]] <- -1 # �ͱ������-1
  tumordata[k, ] <- out
}

# �������������ڻ�ͼ
outdata <- tumordata
outdata[outdata==1] <- "UP" # �߱�����ΪUP
outdata[outdata==-1] <- "DOWN" # �ͱ�����ΪDOWN
outdata[outdata==0] <- "NOCHANGE" #������ΪNOCHANGE
write.csv(outdata,"Ternary.csv",row.names = T,col.names = NA)


### �к���ʹ��ward.D����
hcg <- hclust(dist(tumordata), "ward.D")#�о���
hcs <- hclust(dist(t(tumordata)), "ward.D")#�о���
k <- ClusterNumber # �о�����Ŀ
group <- paste0("C",cutree(hcs,k))
names(group) <- colnames(tumordata)

# ע����Ϣ
annCol <- data.frame(Cluster=group,score=signature.gsva[signaturename,])
# ����TGFBscore��k����Ĳ���(�ǲ�),����pvalue
p <- kruskal.test(annCol$score ~ as.factor(annCol$Cluster))$p.value
colnames(annCol)[2] <- paste0(signaturename,"_score P = ",format(p,digits = 3)) # ��ѧ����������2λ

# ��Ӧע����Ϣ����ɫ
annColors <- list(Cluster = c(C1 = ClusterColor[1],
                              C2 = ClusterColor[2],
                              C3 = ClusterColor[3],
                              C4 = ClusterColor[4]),
                  bluered(64))
names(annColors)[2] <- colnames(annCol)[2]

pct <- paste0(round(rowSums(tumordata == 1)/ncol(tumordata),2) * 100,"%") # �����ϵ�����ٷֱ�
rownames(tumordata) <- paste0(rownames(tumordata)," (",pct,")") # ������������ٷֱ�

# ��ͼ
pheatmap(tumordata,
         cluster_rows = hcg,
         cluster_cols = hcs,
         color = c("#283285","#DEDEDE","#651430"),
         annotation_col = annCol,
         annotation_colors = annColors,
         fontsize_col = 2, # ������С
         fontsize_row = 8, # ������С��Ҳ���Բ���ʾ��
         fontsize = 6, # �����С��Ŀ���ǵ���ͼ���Ĵ�С
         cutree_cols = k, # �и�ͼƬ
         legend_breaks = c(-1,0,1), # �޸�ͼ������ʾλ��
         legend_labels = c("mRNA\ndown-regulation","mRNA\nno-change","mRNA\nup-regulation"), # �޸�ͼ����λ�õ�����
         filename = "Heatmap_TernaryCluster.pdf",width = 7,height = 5)
write.table(annCol, file = "Cluster_score_info.xls", quote = FALSE, sep = "\t", append = FALSE, col.names = TRUE, row.names = TRUE)
```