library(ggplot2)
library(plotROC)

data<-read.csv("new_data_roc1.txt", sep = "\t", header = T)
row.names(data)<- make.names(data[,1], unique = T)
dnam<- data[,-1]
dnam=na.omit(dnam)
head(dnam)

test <- data.frame(D = dnam$BrS, M1=dnam$BS_RA_aVF, M2=dnam$BS_RA_II, M3=dnam$BS_RA_III,
                   stringsAsFactors = FALSE)

longtest <- melt_roc(test, "D", c("M1", "M2","M3"))
head(longtest)

#############

data<-read.csv("new_data_roc.txt", sep = "\t", header = T)
row.names(data)<- make.names(data[,1], unique = T)
dnam<- data[,-1]
dnam=na.omit(dnam)
head(dnam)

test <- data.frame(D = dnam$BrS, M4=dnam$BS_SD_II, M6=dnam$BS_SD_V6,
                   stringsAsFactors = FALSE)

longtest <- melt_roc(test, "D", c("M4","M6"))
head(longtest)


p = ggplot(longtest, aes(d = D, m=M, color = name)) + geom_roc(n.cuts = 0, show.legend = F)+
  scale_color_manual(values = c("red", "green"))

p + style_roc(major.breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
              minor.breaks = c(seq(0, 0.1, by = 0.01), seq(0.9, 1, by = 0.01)),
              guide = TRUE, xlab = "1 - Specificity (FPF)", ylab = "Sensitivity (TPF)") +
  
          
  annotate("text",x = .642, y = .55, label = paste("BS_SD_II =", round(calc_auc(p)$AUC, 3)[1]),
           size =7, color="red")+
  annotate("text",x = .65, y = .47, label = paste("BS_SD_V6 =", round(calc_auc(p)$AUC, 3)[2]),
           size =7, color="green")+
  
  annotate("text",x = .74, y = .62, label = paste("AUC"),
           size =7, color="black")+
    theme(axis.title=element_text(size = 20),
        axis.text=element_text(size = 18))




#############

data<-read.csv("new_data_roc2.txt", sep = "\t", header = T)
row.names(data)<- make.names(data[,1], unique = T)
dnam<- data[,-1]
dnam=na.omit(dnam)
head(dnam)

test <- data.frame(D = dnam$BrS, M7=dnam$BS_RD_aVR, M8=dnam$BS_RD_III,
                   stringsAsFactors = FALSE)

longtest <- melt_roc(test, "D", c("M7","M8"))
head(longtest)


p = ggplot(longtest, aes(d = D, m=M, color = name)) + geom_roc(n.cuts = 0, show.legend = F)+
  scale_color_manual(values = c("red", "green"))

p + style_roc(major.breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
              minor.breaks = c(seq(0, 0.1, by = 0.01), seq(0.9, 1, by = 0.01)),
              guide = TRUE, xlab = "1 - Specificity (FPF)", ylab = "Sensitivity (TPF)") +
  
  
  annotate("text",x = .642, y = .55, label = paste("BS_SD_II =", round(calc_auc(p)$AUC, 3)[1]),
           size =7, color="red")+
  annotate("text",x = .65, y = .47, label = paste("BS_SD_V6 =", round(calc_auc(p)$AUC, 3)[2]),
           size =7, color="green")+
  annotate("text",x = .74, y = .62, label = paste("AUC"),
           size =7, color="black")+
  theme(axis.title=element_text(size = 20),
        axis.text=element_text(size = 18))

###############    Z     #####################
data<-read.csv("new_data_rocZ.txt", sep = "\t", header = T)
row.names(data)<- make.names(data[,1], unique = T)
dnam<- data[,-1]
dnam=na.omit(dnam)
head(dnam)
names(dnam)
test <- data.frame(D = dnam$BrS, M4=dnam$Z_BS_RD_aVR, M5=dnam$Z_BS_SD_aVF, M6=dnam$Z_BS_SD_II, 
                   M7=dnam$Z_BS_SD_V6,
                   stringsAsFactors = FALSE)
longtest <- melt_roc(test, "D", c("M4","M5","M6","M7"))
head(longtest)
p = ggplot(longtest, aes(d = D, m=M, color = name)) + geom_roc(n.cuts = 0, show.legend = F)+
  scale_color_manual(values = c("red", "green","blue","orange"))

p + style_roc(major.breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
              minor.breaks = c(seq(0, 0.1, by = 0.01), seq(0.9, 1, by = 0.01)),
              guide = TRUE, xlab = "1 - Specificity (FPF)", ylab = "Sensitivity (TPF)") +
  
annotate("text",x = .675, y = .33, label = paste("Z_BS_RD_aVR =", round(calc_auc(p)$AUC, 3)[1]),
           size =7, color="red")+
annotate("text",x = .67, y = .27, label = paste("Z_BS_SD_aVF =", round(calc_auc(p)$AUC, 3)[2]),
           size =7, color="green")+
annotate("text",x = .655, y = .46, label = paste("Z_BS_SD_II =", round(calc_auc(p)$AUC, 3)[3]),
           size =7, color="blue")+
annotate("text",x = .66, y = .40, label = paste("Z_BS_SD_V6 =", round(calc_auc(p)$AUC, 3)[4]),
           size =7, color="orange")+
annotate("text",x = .80, y = .52, label = paste("AUC"),
           size =7, color="black")+
  theme(axis.title=element_text(size = 20),
        axis.text=element_text(size = 18))






###########

data<-read.csv("new_data_rocZ1.txt", sep = "\t", header = T)
row.names(data)<- make.names(data[,1], unique = T)
dnam<- data[,-1]
dnam=na.omit(dnam)
head(dnam)

test <- data.frame(D = dnam$BrS, M1=dnam$Z_BS_RA_aVF, M2=dnam$Z_BS_RA_II, M3=dnam$Z_BS_RA_III,
                   stringsAsFactors = FALSE)

longtest <- melt_roc(test, "D", c("M1","M2","M3"))
head(longtest)


p = ggplot(longtest, aes(d = D, m=M, color = name)) + geom_roc(n.cuts = 0, show.legend = F)+
  scale_color_manual(values = c("red", "green","blue"))

p + style_roc(major.breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
              minor.breaks = c(seq(0, 0.1, by = 0.01), seq(0.9, 1, by = 0.01)),
              guide = TRUE, xlab = "1 - Specificity (FPF)", ylab = "Sensitivity (TPF)") +
  
  annotate("text",x = .66, y = .39, label = paste("Z_BS_RA_aVF =", round(calc_auc(p)$AUC, 3)[2]),
           size =7, color="red")+
  annotate("text",x = .662, y = .55, label = paste("Z_BS_RA_II =", round(calc_auc(p)$AUC, 3)[3]),
           size =7, color="green")+
  annotate("text",x = .648, y = .47, label = paste("Z_BS_RA_III =", round(calc_auc(p)$AUC, 3)[1]),
           size =7, color="blue")+

  annotate("text",x = .82, y = .62, label = paste("AUC"),
           size =7, color="black")+
  theme(axis.title=element_text(size = 20),
        axis.text=element_text(size = 18))

