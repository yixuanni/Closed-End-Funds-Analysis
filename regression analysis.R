library(data.table)
data <- fread('cef_analysis1.csv')
data[, ret_2 := NULL]
data <- na.omit(data)
setDT(data, key = c("symbol", "date"))
symbol_groups <- split(data, data$symbol)

library(stats)

regression_analysis <- function(df) {
  df$return <- scale(df$ret_1)
  df$factor_standardized <- scale(df$perm) # can change factors to test
  lm_model <- lm(return ~ factor_standardized, data = df) 
  
  print(summary(lm_model))
  
  plot(df$factor_standardized, df$return, main = "Scatterplot of Factor and Return")
  abline(lm_model, col = "red")
  
  return(lm_model)
}
regression_results <- lapply(symbol_groups, regression_analysis)

data$return <- scale(data$ret_1)
data$factor_standardized <- scale(data$perm) # can change factors to test
model <- lm(return ~ factor_standardized, data = data)
summary(model)
plot(data$factor_standardized, data$return, main = "BIG Scatterplot of Factor and Return")
abline(model, col = "red")

# volume has positive effect on return; need to explore more
# prem has negative effect on return; very significant at all; need to explore more

# for constant variables like ExpenseRatio; Market cap; Current dividend yield;
# Historical average dividend yield; Leverage factor; Average volume

data2 <- fread('summary_return.csv')
setDT(data2)
setindex(data2, fund)
data2

# 定义一个包含因子的列表
set <- c('ExpenseRatio', 'Market cap', 'Current dividend yield', 'Historical average dividend yield', 'Leverage factor', 'Average volume')

# 遍历每个因子
for (i in set) {
  # 创建一个新列，用于存放因子的标准化值
  data2$return <- scale(data2$avg_return1)
  data2[[paste0(i, '_standardized')]] <- scale(data2[[i]])
  
  # 进行线性回归分析
  model <- lm(return ~ ., data = data2[, c('return', paste0(i, '_standardized')), with = FALSE])
  
  # 打印回归结果的摘要
  print(summary(model))
  
  # 创建散点图
  plot(data2[[paste0(i, '_standardized')]], data2$avg_return1, main = paste(i, "-return"))
  
  # 在散点图上添加回归线
  abline(model, col = "red")
}
  