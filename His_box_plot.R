#需要customLayout包来布局，如果没有要通过下面命令安装。
#install.packages("customLayout")
library(customLayout)
His_box_plot <- function(x){
 lay <- lay_new(
  mat = matrix(1:2, ncol = 1),
  heights = c(3, 2))  
lay_show(lay)
hist(x,freq = FALSE,col = '3',xlab='data',ylab = 'Frequency',main = 'Histogram of data')
lines(density(x),col='2',lwd = 2)
rug(jitter(x),col='4',side=1)
boxplot(x, horizontal = TRUE,notch=TRUE,varwidth=TRUE,col = '3', border="2",xlab = 'data',main = "Boxplot of data")
}

data <- c(81.0, 72.0, 61.7,80.3 ,72.0 ,61.4,80.0 , 71.7, 60.5 ,79.5 , 71.4,
 59.4,79.4,71.4 , 58.6,79.4, 71.1,58.1,78.7 , 71.1 ,58.1 ,78.5 , 70.7 ,
 56.5,78.3 , 70.4, 50.7 ,77.4 ,70.4 , 50.7 ,77.1 , 69.7, 50.4 ,76.7,
 69.7, 48.7 ,76.4, 69.6 , 48.4 ,76.0, 69.3, 48.0 ,75.7 , 69.2 , 47.2 ,
75.2, 68.8 ,46.5 ,74.9 , 68.3 , 44.9 ,74.7, 66.8 , 44.1 ,74.1 ,
66.1 ,42.5, 72.9 , 64.1 , 41.2 ,72.7 , 64.0, 40.9 ,72.1, 62.6 , 39.0 )
His_box_plot(data)
#只需输入数据，即可出图,这里使用了论文中的数据，
#改成其他数据也可，可能会需要参数的微调。
