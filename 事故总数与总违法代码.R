#本代码功能：
# 针对欠执法的中队和大队，分别以日和时段为单位，
#使用LOESS进行拟合,绘制出非参数拟合曲线
library("rucrdtw")
setwd("D:/3-各类数据/R-data/4-项目/0-苏州项目-4个月数据")
dat1 <- read.csv("天.csv", header = T)
dat2 <- read.csv("时.csv", header = T)
name <- dat1$大队名称
l <- length(name)

# 创建字符串数据，表示欠执法的大队
poorDadui <- c("昆山大队", "常熟大队", "相城大队", "园区大队")
#找出第n个大队大队的起点和终点序号，为i,j
for (n in 1:4) {
  
  for (i in 1:l) {
    if (poorDadui[n] == name[i]) {
      break
      
    }
  }
  for (j in i:l) {
    if (poorDadui[n] != name[j]) {
      break
      
    }
  }
  j <- j - 1
  
  #第n大队的数据准备
  x <- dat1$天[i:j]
  y1 <- dat1$事故数量[i:j]
  y2 <- dat1$违法数量[i:j]
  #绘制第n大队的散点图并拟合曲线——总事故数
  lw1 <- loess(y1 ~ x)
  plot(
    y1 ~ x,
    pch = 19,
    cex = 1,
    xlab = "日期",
    ylab = "总事故数（起）"
  )
  title(main = poorDadui[n] )
  j <- order(x)
  lines(x[j], lw1$fitted[j], col = "red", lwd = 3)
  #绘制第n大队的散点图并拟合曲线——总违法数
  lw1 <- loess(y2 ~ x)
  plot(
    y1 ~ x,
    pch = 19,
    cex = 1,
    xlab = "日期",
    ylab = "总违法数（起）"
  )
  title(main = poorDadui[n] )
  j <- order(x)
  lines(x[j], lw1$fitted[j], col = "red", lwd = 3)
  # 对时间进行处理
  # 将name和l的值进行替换
  name <- dat2$大队名称
  l <- length(name)
  for (i in 1:l) {
    if (poorDadui[n] == name[i]) {
      break
      
    }
  }
  for (j in i:l) {
    if (poorDadui[n] != name[j]) {
      break
      
    }
  }
  j <- j - 1
  
  #第n大队的数据准备
  x <- dat2$时[i:j]
  y1 <- dat1$事故数量[i:j]
  y2 <- dat1$违法数量[i:j]
  #绘制第n大队的散点图并拟合曲线——总事故数
  lw1 <- loess(y1 ~ x)
  plot(
    y1 ~ x,
    pch = 19,
    cex = 1,
    xlab = "小时段",
    ylab = "总事故数（起）"
  )
  title(main = poorDadui[n] )
  j <- order(x)
  lines(x[j], lw1$fitted[j], col = "red", lwd = 3)
  #绘制第n大队的散点图并拟合曲线——总违法数
  lw1 <- loess(y2 ~ x)
  plot(
    y1 ~ x,
    pch = 19,
    cex = 1,
    xlab = "小时段",
    ylab = "总违法数（起）"
  )
  title(main = poorDadui[n] )
  j <- order(x)
  lines(x[j], lw1$fitted[j], col = "red", lwd = 3)
}

