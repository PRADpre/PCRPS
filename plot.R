
#=======================================================

#=======================================================
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require(reshape2)) {
  install.packages("reshape2")
  library(reshape2)
}

rm(list=ls())

# 找到该目录下的所有CSV文件
csv_files <- list.files(pattern = "*.csv")


i = 1
data1 <- read.csv(csv_files[i], row.names = 1)
data1$GeneSet <- paste0("article", data1$GeneSet)
data1 <- subset( data1, select = c( 'GeneSet' ,      'AUC'))
names(data1)[2] <- gsub(".csv", "", csv_files[i])



for (i in 2:9) {
  
  data2 <- read.csv(csv_files[i], row.names = 1)
  data2$GeneSet <- paste0("article", data2$GeneSet)
  data2 <- subset( data2, select = c( 'GeneSet' ,      'AUC'))
  names(data2)[2] <- gsub(".csv", "", csv_files[i])
  data1 <- merge(data1, data2, by="GeneSet")
  
}


df <- data1
rownames(df) <- df$GeneSet
df$GeneSet <- NULL

df <- round(df, 3)


# 对每一列进行操作，生成柱状图并保存
for (col_name in colnames(df)) {
  
  # 创建数据子集，并对文章名称根据对应的值进行排序
  subset <- data.frame(Article = reorder(rownames(df), df[, col_name], FUN = median), Value = df[, col_name])
  
  # 绘制柱状图
  p <- ggplot(subset, aes(x=Article, y=Value, fill=(Article=="article23"))) +
    geom_bar(stat="identity") +
    geom_text(aes(label=Value), vjust=-0.3, size=2) +
    scale_fill_manual(values = c("#98D8AA", "#F9D949")) +
    labs(y="Article", x="Value", title=paste("Dataset", col_name)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          text = element_text(size=6), 
          axis.text.x = element_text(angle=90, hjust=1),
          panel.border = element_rect(fill = NA, color = "black"),
          legend.position = "none") + xlab("") + ylab("")+
    coord_flip()

  
  # 保存图像为PDF格式
  ggsave(paste("barplot_", col_name, ".pdf", sep=""), plot=p, width=2, height=6)
}