setwd("C:")
getwd()
library(dplyr)
library(tidyverse)
library(tidymodels)
library(rstatix)
library(skimr)

# Kaggle 데이터 활용
# 심장 마비 데이터 : https://www.kaggle.com/datasets/rashikrahmanpritom/heart-attack-analysis-prediction-dataset?resource=download





# <가설> 심장 박동 수가 높은 사람일수록 심장 마비 가능성이 높다 #





# 1. 데이터 불러오기
heart_data <- read.csv("heart.csv")

head(heart_data)
View(heart_data)
str(heart_data)
glimpse(heart_data)
summary(heart_data)



# 2. 필요한 데이터만 분리하기
sep_data <- subset(heart_data,select=c(age, cp, thalachh, output))

summary(sep_data)



# 3. 변수 별 이상치 결측치 확인
sep_data %>%
  ggplot(aes(y = age)) +
  geom_boxplot()+
  labs(y = "age")

sep_data %>% 
  identify_outliers

table(is.na(sep_data$age))


ggplot(sep_data, aes(x = cp)) + geom_bar()

table(is.na(sep_data$cp))


sep_data %>%
  ggplot(aes(y = thalachh)) +
  geom_boxplot()+
  labs(y = "thalachh")

sep_data %>% 
  identify_outliers(thalachh)

table(is.na(sep_data$thalachh))



# 4. 이상치 제거
sep_data <- sep_data %>%
  filter(!(thalachh<=71))

length(sep_data$thalachh)

sep_data %>%
  ggplot(aes(y = thalachh)) +
  geom_boxplot()+
  labs(y = "thalachh")

sep_data %>% 
  identify_outliers(thalachh)





# <추가 가설> 심장 마비의 사람들의 최대 심장 박동 수는 150 이상일 것이다. #





heart_sep_data = subset(sep_data, sep_data$output == 1)
  
heart_sep_data %>%
  shapiro_test(thalachh)

options(scipen=100) #지수표기가 아닌 일반숫자표기를 위한 코드 
heart_sep_data %>%
  t_test(formula = thalachh ~ 1,
         alternative = "greater",
         mu = 150.0,
         conf.level = 0.95,
         detailed = TRUE) #우측검정



# 6. 최대 심박수와 심장 마비 데이터 상관 분석
heart_tha <- sep_data %>% 
  group_by(output) %>% 
  summarize(thalachh_mean = mean(thalachh))

heart_tha

ggplot(data = heart_tha, aes(x = output,
                              y = thalachh_mean)) +
  geom_col()
