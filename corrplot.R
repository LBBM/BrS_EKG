library(PerformanceAnalytics)
library(tidyverse)

data<-read.csv("new_data_3.txt", sep = "\t", header = T)
data=as.data.frame(na.omit(data))
dim(data)
data=data[,c(2,52,56,57,92,98,99)]

data=data %>% 
  filter(BrS %in% c("No"))
data <- log(data[,2:7])
chart.Correlation(data, histogram=TRUE, pch=19)

data=data %>% 
  filter(BrS %in% c("Yes"))
data <- log(data[,2:7])
chart.Correlation(data, histogram=TRUE, pch=19)


