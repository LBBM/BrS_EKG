library(tidyverse)
library(readxl)
library(ggplot2)
library(glue)
library(ggtext)
library(ggpubr)

data<-read.csv("new_data_3.txt", sep = "\t", header = T)
data=data[,c(2,52,56,57,92,98,99)]
names(data)
T1_n = data %>% 
  filter(BrS == "No") %>% 
  nrow()
T2_n = data %>% 
  filter(BrS == "Yes") %>% 
  nrow()

data$BrS <- factor(data$BrS, levels = c("No", "Yes"))

p1 <- data %>%
  ggplot(aes(x=BrS, BS_RA_aVF, fill = BrS))+
  geom_boxplot(show.legend = F, outlier.shape = NA, alpha=0.5, width=0.6, coef=0)+
  geom_jitter(aes(shape = BrS, color = BrS), size = 1.8, show.legend = T, width=0.15)+
  guides(fill = "none")+
  stat_summary(fun = median, show.legend=F, geom='crossbar', width=0.6, size=0.5, color='black')+
  labs(x=NULL, y="BS_RA_aVF")+
  scale_x_discrete(breaks = c("No","Yes"), labels = c(glue("non-BrS (N={T1_n})"),glue("BrS (N={T2_n})")))+
  scale_fill_grey(start = 1, end = 0)+
  scale_color_manual(values = c("steelblue3","coral3"))+
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 11, color = "black", face = "bold"),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),legend.text = element_blank())
compare_means(BS_RA_aVF ~ BrS, data = data, method = "wilcox.test")
my_comparisons <- list( c("No", "Yes"))
p1 = p1 + stat_compare_means(comparisons = my_comparisons,label.y = 2100)
p1

p2 <- data %>%
  ggplot(aes(x=BrS, BS_RA_II, fill = BrS))+
  geom_boxplot(show.legend = F, outlier.shape = NA, alpha=0.5, width=0.6, coef=0)+
  geom_jitter(aes(shape = BrS, color = BrS), size = 1.8, show.legend = T, width=0.15)+
  guides(fill = "none")+
  stat_summary(fun = median, show.legend=F, geom='crossbar', width=0.6, size=0.5, color='black')+
  labs(x=NULL, y="BS_RA_II")+
  scale_x_discrete(breaks = c("No","Yes"), labels = c(glue("non-BrS (N={T1_n})"),glue("BrS (N={T2_n})")))+
  scale_fill_grey(start = 1, end = 0)+
  scale_color_manual(values = c("steelblue3","coral3"))+
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 11, color = "black", face = "bold"),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),legend.text = element_blank())
compare_means(BS_RA_II ~ BrS, data = data, method = "wilcox.test")
my_comparisons <- list( c("No", "Yes"))
p2 = p2 + stat_compare_means(comparisons = my_comparisons,label.y = 2100)
p2


p3 <- data %>%
  ggplot(aes(x=BrS, BS_RA_III, fill = BrS))+
  geom_boxplot(show.legend = F, outlier.shape = NA, alpha=0.5, width=0.6, coef=0)+
  geom_jitter(aes(shape = BrS, color = BrS), size = 1.8, show.legend = T, width=0.15)+
  guides(fill = "none")+
  stat_summary(fun = median, show.legend=F, geom='crossbar', width=0.6, size=0.5, color='black')+
  labs(x=NULL, y="BS_RA_III")+
  scale_x_discrete(breaks = c("No","Yes"), labels = c(glue("non-BrS (N={T1_n})"),glue("BrS (N={T2_n})")))+
  scale_fill_grey(start = 1, end = 0)+
  scale_color_manual(values = c("steelblue3","coral3"))+
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 11, color = "black", face = "bold"),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),legend.text = element_blank())
compare_means(BS_RA_III ~ BrS, data = data, method = "wilcox.test")
my_comparisons <- list( c("No", "Yes"))
p3 = p3 + stat_compare_means(comparisons = my_comparisons,label.y = 2100)
p3

p5 <- data %>%
  ggplot(aes(x=BrS, BS_SD_II, fill = BrS))+
  geom_boxplot(show.legend = F, outlier.shape = NA, alpha=0.5, width=0.6, coef=0)+
  geom_jitter(aes(shape = BrS, color = BrS), size = 1.8, show.legend = T, width=0.15)+
  guides(fill = "none")+
  stat_summary(fun = median, show.legend=F, geom='crossbar', width=0.6, size=0.5, color='black')+
  labs(x=NULL, y="BS_SD_II")+
  scale_x_discrete(breaks = c("No","Yes"), labels = c(glue("non-BrS (N={T1_n})"),glue("BrS (N={T2_n})")))+
  scale_fill_grey(start = 1, end = 0)+
  scale_color_manual(values = c("steelblue3","coral3"))+
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 11, color = "black", face = "bold"),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),legend.text = element_blank())+
  ylim(0,170)
compare_means(BS_SD_II ~ BrS, data = data, method = "wilcox.test")
my_comparisons <- list( c("No", "Yes"))
p5 = p5 + stat_compare_means(comparisons = my_comparisons,label.y = 130)
p5

p6 <- data %>%
  ggplot(aes(x=BrS, BS_SD_V5, fill = BrS))+
  geom_boxplot(show.legend = F, outlier.shape = NA, alpha=0.5, width=0.6, coef=0)+
  geom_jitter(aes(shape = BrS, color = BrS), size = 1.8, show.legend = T, width=0.15)+
  guides(fill = "none")+
  stat_summary(fun = median, show.legend=F, geom='crossbar', width=0.6, size=0.5, color='black')+
  labs(x=NULL, y="BS_SD_V5")+
  scale_x_discrete(breaks = c("No","Yes"), labels = c(glue("non-BrS (N={T1_n})"),glue("BrS (N={T2_n})")))+
  scale_fill_grey(start = 1, end = 0)+
  scale_color_manual(values = c("steelblue3","coral3"))+
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 11, color = "black", face = "bold"),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),legend.text = element_blank())+
  ylim(0,170)
compare_means(BS_SD_V5 ~ BrS, data = data, method = "wilcox.test")
my_comparisons <- list( c("No", "Yes"))
p6 = p6 + stat_compare_means(comparisons = my_comparisons,label.y = 140)
p6

p7 <- data %>%
  ggplot(aes(x=BrS, BS_SD_V6, fill = BrS))+
  geom_boxplot(show.legend = F, outlier.shape = NA, alpha=0.5, width=0.6, coef=0)+
  geom_jitter(aes(shape = BrS, color = BrS), size = 1.8, show.legend = T, width=0.15)+
  guides(fill = "none")+
  stat_summary(fun = median, show.legend=F, geom='crossbar', width=0.6, size=0.5, color='black')+
  labs(x=NULL, y="BS_SD_V6")+
  scale_x_discrete(breaks = c("No","Yes"), labels = c(glue("non-BrS (N={T1_n})"),glue("BrS (N={T2_n})")))+
  scale_fill_grey(start = 1, end = 0)+
  scale_color_manual(values = c("steelblue3","coral3"))+
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 11, color = "black", face = "bold"),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),legend.text = element_blank())+
  ylim(0,150)
compare_means(BS_SD_V6 ~ BrS, data = data, method = "wilcox.test")
my_comparisons <- list( c("No", "Yes"))
p7 = p7 + stat_compare_means(comparisons = my_comparisons,label.y = 120)
p7


####################       Z            #####################


data<-read.csv("new_data_3Z.txt", sep = "\t", header = T)
data=data[,c(2,49,53,54,63,85,89,96)]
names(data)
T1_n = data %>% 
  filter(BrS == "No") %>% 
  nrow()
T2_n = data %>% 
  filter(BrS == "Yes") %>% 
  nrow()

data$BrS <- factor(data$BrS, levels = c("No", "Yes"))

p1 <- data %>%
  ggplot(aes(x=BrS, Z_BS_SD_II, fill = BrS))+
  geom_boxplot(show.legend = F, outlier.shape = NA, alpha=0.5, width=0.6, coef=0)+
  geom_jitter(aes(shape = BrS, color = BrS), size = 1.8, show.legend = T, width=0.15)+
  guides(fill = "none")+
  stat_summary(fun = median, show.legend=F, geom='crossbar', width=0.6, size=0.5, color='black')+
  labs(x=NULL, y="Z_BS_SD_II")+
  scale_x_discrete(breaks = c("No","Yes"), labels = c(glue("non-BrS (N={T1_n})"),glue("BrS (N={T2_n})")))+
  scale_fill_grey(start = 1, end = 0)+
  scale_color_manual(values = c("steelblue3","coral3"))+
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 11, color = "black", face = "bold"),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),legend.text = element_blank())+
  ylim(-2,11)
compare_means(Z_BS_SD_II ~ BrS, data = data, method = "wilcox.test")
my_comparisons <- list( c("No", "Yes"))
p1 = p1 + stat_compare_means(comparisons = my_comparisons,label.y = 8)
p1

p3 <- data %>%
  ggplot(aes(x=BrS, Z_BS_SD_V6, fill = BrS))+
  geom_boxplot(show.legend = F, outlier.shape = NA, alpha=0.5, width=0.6, coef=0)+
  geom_jitter(aes(shape = BrS, color = BrS), size = 1.8, show.legend = T, width=0.15)+
  guides(fill = "none")+
  stat_summary(fun = median, show.legend=F, geom='crossbar', width=0.6, size=0.5, color='black')+
  labs(x=NULL, y="Z_BS_SD_V6")+
  scale_x_discrete(breaks = c("No","Yes"), labels = c(glue("non-BrS (N={T1_n})"),glue("BrS (N={T2_n})")))+
  scale_fill_grey(start = 1, end = 0)+
  scale_color_manual(values = c("steelblue3","coral3"))+
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 11, color = "black", face = "bold"),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),legend.text = element_blank())+
  ylim(-2,11)
compare_means(Z_BS_SD_V6 ~ BrS, data = data, method = "wilcox.test")
my_comparisons <- list( c("No", "Yes"))
p3 = p3 + stat_compare_means(comparisons = my_comparisons,label.y = 9)
p3

p5 <- data %>%
  ggplot(aes(x=BrS, Z_BS_SD_aVF, fill = BrS))+
  geom_boxplot(show.legend = F, outlier.shape = NA, alpha=0.5, width=0.6, coef=0)+
  geom_jitter(aes(shape = BrS, color = BrS), size = 1.8, show.legend = T, width=0.15)+
  guides(fill = "none")+
  stat_summary(fun = median, show.legend=F, geom='crossbar', width=0.6, size=0.5, color='black')+
  labs(x=NULL, y="Z_BS_SD_aVF")+
  scale_x_discrete(breaks = c("No","Yes"), labels = c(glue("non-BrS (N={T1_n})"),glue("BrS (N={T2_n})")))+
  scale_fill_grey(start = 1, end = 0)+
  scale_color_manual(values = c("steelblue3","coral3"))+
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 11, color = "black", face = "bold"),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),legend.text = element_blank())+
  ylim(-2,10)
compare_means(Z_BS_SD_aVF ~ BrS, data = data, method = "wilcox.test")
my_comparisons <- list( c("No", "Yes"))
p5 = p5 + stat_compare_means(comparisons = my_comparisons,label.y = 7)
p5


p9 <- data %>%
  ggplot(aes(x=BrS, Z_BS_RD_aVR, fill = BrS))+
  geom_boxplot(show.legend = F, outlier.shape = NA, alpha=0.5, width=0.6, coef=0)+
  geom_jitter(aes(shape = BrS, color = BrS), size = 1.8, show.legend = T, width=0.15)+
  guides(fill = "none")+
  stat_summary(fun = median, show.legend=F, geom='crossbar', width=0.6, size=0.5, color='black')+
  labs(x=NULL, y="Z_BS_RD_aVR")+
  scale_x_discrete(breaks = c("No","Yes"), labels = c(glue("non-BrS (N={T1_n})"),glue("BrS (N={T2_n})")))+
  scale_fill_grey(start = 1, end = 0)+
  scale_color_manual(values = c("steelblue3","coral3"))+
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 11, color = "black", face = "bold"),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),legend.text = element_blank())+
  ylim(-3,12)
compare_means(Z_BS_RD_aVR ~ BrS, data = data, method = "wilcox.test")
my_comparisons <- list( c("No", "Yes"))
p9 = p9 + stat_compare_means(comparisons = my_comparisons,label.y = 10)
p9

