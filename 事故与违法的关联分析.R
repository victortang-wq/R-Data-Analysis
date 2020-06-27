#导入事故与违法数据
library("rucrdtw")
setwd("D:/3-各类数据/R-data/4-项目/0-苏州项目-4个月数据")
dat1<-read.csv("中队事故与违法总数.csv",header=T)
x <- dat1$事故数量
y <- dat1$违法数量
l <- length(x)
#剔除事故数为0的中队
for (j in 1:l) {
  if(x[j]>25){
    break;
  }
}
dat1 <- dat1[j:l,];
#重新确定行数
x <- dat1$事故数量
y <- dat1$违法数量
l <- length(x)
#绘制标准化后的事故与违法散点图
x2<- (x-mean(x))/sd(x)
y2<- (y-mean(y))/sd(y)
plot(y2 ~ x2, pch=19,cex=1,xlab= "总事故数（标准化后）",ylab = "总违法数（标准化后）")
#设置坐标轴范围
# axis(1,-1:2,-1:2)
# axis(2,-1:2,-1:1.5)
#在图上添加文本，注释
for (i in 1:l) {
  text(x2[i],y2[i],dat1$大队[i])
}
#绘制一条对角线
# abline(h=1)
#cor.test(dat1[1:10,n],dat2[1:10,n],method="pearson")

#model1<-loess(dat3$事故总数~dat3$违法总数)

#cor(dat2$苏州市公安局相城分局交通警察大队,dat1$苏州市公安局相城分局交通警察大队)
#cov(dat2$苏州市公安局相城分局交通警察大队,dat1$苏州市公安局相城分局交通警察大队)
# 
# y <- dat3$总违法
# x <- dat3$总事故
# cor(x,y)
# cov(x,y)
# 
# lw1 <- loess(y ~ x)
# plot(y ~ x, pch=19,cex=1,xlab= "总事故数（起）",ylab = "总违法数（起）")
# j <- order(x)
# lines(x[j],lw1$fitted[j],col="red",lwd=3)