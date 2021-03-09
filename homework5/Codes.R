### 1
setwd("E:\\大学\\课程\\专业必修\\大三上\\统计机器学习\\HW\\HW5\\市长电话分析")
train.data <- read.csv(file="train_set.csv", header=T)
test.data <- read.csv(file="test_set.csv", header=T)

library(ggplot2)
ggplot(train.data, aes(x=reorder(单位名称, rep(-1, length(单位名称)), sum))) + 
  geom_bar() + labs(title="各单位接到投诉量柱状图",x="政府部门", y = "投诉量(条)")


### 2
train.data$rowSum <- rowSums(train.data[,-1])
ggplot(train.data, aes(x=rowSum)) + geom_bar(width = 2)


### 3
ggplot(train.data, aes(x=reorder(单位名称, rowSum, median), y=rowSum)) + 
  geom_boxplot() + labs(title="政府单位-投诉信息词汇量 分组箱线图",x="政府单位", y = "投诉信息词汇量(个)")


### 4 
train.data[, -1] <- train.data[, -1] > 0
test.data[, -1] <- test.data[, -1] > 0
library(e1071)
model <- naiveBayes(单位名称 ~ ., data = train.data)
pred <- predict(model, test.data[, -1])
mx <- as.data.frame(table(true.class = test.data$单位名称, predict.class = pred))
ggplot(mx, aes(predict.class, true.class)) + 
  geom_tile(aes(fill = Freq), color = "white") + 
  scale_fill_gradient(low = "white", high = "blue") + 
  geom_text(aes(label=Freq), color="black") + labs(title = '训练集预测混淆矩阵')
