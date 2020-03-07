mse_bias_res <- read.table("E:\\data\\scale_test_paper_power\\estimator_mse_bias.txt")
mse_bias_res 
mse_seq <- seq(1,dim(mse_bias_res)[1],2)
bias_seq <- seq(2,dim(mse_bias_res)[1],2)
mse_res <- mse_bias_res[mse_seq,-1]
bias_res <- mse_bias_res[bias_seq,-1]
scheme_nu <- seq(1,28)

rownames(mse_res) <-paste("C.S number ", 1:28, sep = " ")
rownames(bias_res) <- paste("C.S number ", 1:28, sep = " ")

colnames(mse_res) <- c('theta_hat_ED','theta_hat_RD','theta_hat_HLD')
colnames(bias_res) <- c('theta_hat_ED','theta_hat_RD','theta_hat_HLD')
mse_res;bias_res
library(pheatmap)
library(reshape2)
library(ggplot2)
library(gplots)
pheatmap(mse_res,display_numbers = TRUE,number_format = "%.5f",fontsize = 30,fontsize_number = 0.8 * 30,cluster_rows =FALSE,cluster_cols = FALSE,main = 'The heatmap of MSE')
pheatmap(bias_res,display_numbers = TRUE,number_format = "%.5f",fontsize = 30,fontsize_number = 0.8 * 30,cluster_rows =FALSE,cluster_cols = FALSE,main = 'The heatmap of bias')
# number_format = "%.2f" 保留2位小数





test = matrix(rnorm(200), 20, 10)
test[1:10, seq(1, 10, 2)] = test[1:10, seq(1, 10, 2)] + 3
test[11:20, seq(2, 10, 2)] = test[11:20, seq(2, 10, 2)] + 2
test[15:20, seq(2, 10, 2)] = test[15:20, seq(2, 10, 2)] + 4
colnames(test) = paste("Test", 1:10, sep = "")
rownames(test) = paste("Gene", 1:20, sep = "")


# Draw heatmaps
pheatmap(test)
pheatmap(test, kmeans_k = 2)
pheatmap(test, scale = "row", clustering_distance_rows = "correlation")
pheatmap(test, color = colorRampPalette(c("navy", "white", "firebrick3"))(50))
pheatmap(test, cluster_row = FALSE)
pheatmap(test, legend = FALSE)

# Show text within cells
pheatmap(test, display_numbers = TRUE)
pheatmap(test, display_numbers = TRUE, number_format = "\%.1e")
pheatmap(test, display_numbers = matrix(ifelse(test > 5, "*", ""), nrow(test)))

ann_colors = list(
  Time = c("white", "firebrick"),
  CellType = c(CT1 = "#1B9E77", CT2 = "#D95F02"),
  GeneClass = c(Path1 = "#7570B3", Path2 = "#E7298A", Path3 = "#66A61E")
)

pheatmap(test, annotation_col = annotation_col, annotation_colors = ann_colors, main = "Title")
pheatmap(test, annotation_col = annotation_col, annotation_row = annotation_row, 
         annotation_colors = ann_colors)
pheatmap(test, annotation_col = annotation_col, annotation_colors = ann_colors[2]) 


