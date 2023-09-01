
#########1.Data Preparation#########

###1.1.import packages needed###

library(caTools) 


###1.2.Import data###
data <- readRDS("C://Users//HUAWEI//Documents//IIDS67650D//SMARTs_medinfo_P1.rds")
data


###1.3.Modify Data Type###

data$outcome <- as.factor(data$outcome)
data$EVENT <- as.factor(data$EVENT)
data$albumin <- as.factor(data$albumin)
data$SMOKING <- as.factor(data$SMOKING)
data$alcohol <- as.factor(data$alcohol)



###1.4.Export Dataset###
##############################


#Dataset with missing values
develop_set_mv <- data
write.csv(develop_set_mv,file="C://Users//HUAWEI//Documents//IIDS67650D//20230711meeting//develop_set_mv.csv",row.names = F)


#Delete Missing Values
develop_set_cc = data[complete.cases(data),]#从data中提取出完整观测
write.csv(develop_set_cc,file="C://Users//HUAWEI//Documents//IIDS67650D//20230711meeting//develop_set_cc.csv",row.names = F)
####################


#########2.Descriptive statistics#########


###2.1.Description###

summary(data)
summary(data$TEVENT, data$AGE)

##2.1.1.Descriptive Statistics of Numeric Variables
numeric_vars <- sapply(data, is.numeric)
# Summarize numerical variables
summary(data[, numeric_vars])

##2.1.2.Descriptive Statistics of Categorical Variables
library(ggplot2)
categorical_vars <- sapply(data, is.factor)
# Draw distribution histograms
for (var in names(data)[categorical_vars]) {
  p <- ggplot(data, aes_string(x = var)) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    labs(title = paste("Distribution of", var),
         x = var,
         y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}


###2.2.Normality test###
library(stats)

# Selecting Numeric Variables
numeric_vars <- sapply(data, is.numeric)

# Perform Shapiro-Wilk test
for (var in names(data)[numeric_vars]) {
  shapiro_test <- shapiro.test(data[[var]])
  
  print(paste("Variable:", var))
  print(shapiro_test)
}


###2.3.Single-factor Analysis###
predictor_vars <- c("EVENT", "TEVENT", "SEX", "AGE", "DIABETES",
                    "CEREBRAL", "CARDIAC", "AAA", "PERIPH", "STENOSIS",
                    "SYSTBP", "DIASTBP", "SYSTH", "DIASTH", "LENGTHO",
                    "WEIGHTO", "BMIO", "CHOLO", "albumin", "SMOKING", "packyrs", "alcohol")

data1 <- na.omit(data)

# Create dummy variable encoding
data1_factors <- data1[, c("EVENT","SEX", "DIABETES", "CEREBRAL", "CARDIAC", "AAA", "PERIPH", "STENOSIS", "albumin", "SMOKING", "alcohol")]
dummy_vars <- model.matrix(~ . - 1, data = data1_factors)

# Add dummy variables to the dataset
data1 <- cbind(data1, dummy_vars)

# 逐步比较校正
alpha <- 0.05 / length(predictor_vars)  # Bonferroni校正后的显著性水平

for (var in predictor_vars) {
  formula <- as.formula(paste("outcome ~ ", var))
  model <- lm(formula, data1)
  anova_result <- anova(model)
  
  if (anova_result$"Pr(>F)"[1] < alpha) {
    print(paste("Single Factor Analysis for:", var))
    print(anova_result)
  }
}

###2.4.Comparison###

# List of Numeric Variables
numeric_vars <- c("TEVENT", "AGE", "SYSTBP", "DIASTBP", "SYSTH", "DIASTH",
                  "LENGTHO", "WEIGHTO", "BMIO", "CHOLO", "packyrs")

# List of Categorical Variables
categorical_vars <- c("EVENT","SEX", "DIABETES", "CEREBRAL", "CARDIAC", "AAA",
                      "PERIPH", "STENOSIS", "albumin", "SMOKING", "alcohol")
# Divide theoutcome into two subsets
data_outcome_1 <- data[data$outcome == 1, ]
data_outcome_0 <- data[data$outcome == 0, ]

# Comparing mean and standard deviation (numerical variables)
for (var in numeric_vars) {
  mean_1 <- mean(data_outcome_1[[var]], na.rm = TRUE)
  mean_0 <- mean(data_outcome_0[[var]], na.rm = TRUE)
  
  sd_1 <- sd(data_outcome_1[[var]], na.rm = TRUE)
  sd_0 <- sd(data_outcome_0[[var]], na.rm = TRUE)
  
  z_value <- (mean_1 - mean_0) / sqrt((sd_1^2 / length(data_outcome_1)) + (sd_0^2 / length(data_outcome_0)))
  p_value <- 2 * (1 - pnorm(abs(z_value)))
  
  print(paste("Variable:", var))
  print(paste("Mean (Outcome 1):", mean_1))
  print(paste("Mean (Outcome 0):", mean_0))
  print(paste("Standard Deviation (Outcome 1):", sd_1))
  print(paste("Standard Deviation (Outcome 0):", sd_0))
  print(paste("Z Value:", z_value))
  print(paste("P Value:", p_value))
}

# Comparison of proportions and chi square tests (categorical variables)
for (var in categorical_vars) {
  table_1 <- table(data_outcome_1[[var]])
  table_0 <- table(data_outcome_0[[var]])
  
  prop_1 <- prop.table(table_1)
  prop_0 <- prop.table(table_0)
  
  chi_squared <- chisq.test(table_1, table_0)
  
  print(paste("Variable:", var))
  print("Outcome 1:")
  print(table_1)
  print(prop_1)
  print("Outcome 0:")
  print(table_0)
  print(prop_0)
  print("Chi-Squared Test:")
  print(chi_squared)
}


###2.5.Missing pattern###
library(tidyverse)
library(plyr)

library(DataExplorer)
DataExplorer::plot_missing(data)#缺失数据百分比

library(mice)
md.pattern(data, cex=1.5)#缺失数据pattern

#调整一下能看清
DD_Q2 <- plyr::rename(data, c(TEVENT="A", EVENT="B",
                              SEX="C", AGE="D", DIABETES="E",
                              CEREBRAL="F",CARDIAC="G",
                              AAA="H",PERIPH="I",STENOSIS="J",
                              SYSTBP="K",DIASTBP="L",
                              SYSTH="M",DIASTH="N",
                              LENGTHO="O",WEIGHTO="P",
                              BMIO="Q",CHOLO="R",albumin="S",
                              SMOKING="T",packyrs="U",
                              alcohol="V", outcome="W"))
pdf("output_plot简化.pdf", width = 10, height = 8)
md.pattern(DD_Q2)
dev.off()

md.pairs(data)

##A separate study on four blood pressures
subset_data <- data[, c("SYSTBP", "DIASTBP", "SYSTH", "DIASTH")]
md.pattern(subset_data)

pdf("output_plot血压.pdf", width = 10, height = 8)
md.pattern(subset_data)
dev.off()
##


###2.6.Differences between groups###
# 假设你的数据如下：
outcome_1 <- c(Current = 0, Former = 212)
outcome_0 <- c(Current = 3413, Former = 248)

# 执行卡方检验
chisq_result <- chisq.test(matrix(c(outcome_1, outcome_0), nrow = 2, byrow = TRUE))

# 输出卡方检验结果
print("Chi-Squared Test Result:")
print(chisq_result)




###2.7.Coefficient of Variation###
# Calculation of coefficient of variation
coefficient_of_variation <- function(x) {
  sd_x <- sd(x)
  mean_x <- mean(x)
  cv <- (sd_x / mean_x) * 100
  return(cv)
}
# 应用函数计算每列的变异系数
cv_result <- sapply(data[, numeric_vars], coefficient_of_variation)

# 打印每列的变异系数
print(cv_result)


# 计算每一列的标准差
standard_deviations <- sapply(data[, numeric_vars], sd)

# 打印每一列的标准差
print(standard_deviations)
