#Summary results

data=read.delim2("new_data.txt", sep = "\t", header = T)
head(data)
names(data)
table(data$Type1ECG)
table(data$BrS)
table(data$Gene_status, data$BrS)



data=read.delim2("new_data_3.txt", sep = "\t", header = T)
dim(data)
names(data)
table(data$BrS)

mm<- data.matrix(data[,-1])
dim(mm)
data_mm=na.omit(mm)
data_mm=data_mm[ , which(apply(data_mm, 2, var) != 0)]
dim(data_mm)

res.pca <- prcomp(data_mm, scale = T)
library(factoextra)
library(viridis)
p3= fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 95))+
  theme(axis.text=element_text(size=18), 
        axis.title=element_text(size=18),
        legend.title = element_text(size = 18),
        legend.text=element_text(size=18),
        plot.title = element_blank())
p3

p2=fviz_pca_var(res.pca, col.var="contrib", select.var = list(contrib = 5), repel = TRUE, arrowsize = 1)+
  scale_colour_gradient2(low="#00AFBB", mid="#E7B800", high="#FC4E07") +
  theme(axis.text=element_text(size=18), 
        axis.title=element_text(size=18),
        legend.title = element_text(size = 18),
        legend.text=element_text(size=18),
        plot.title = element_blank())
p2

p1=fviz_pca_biplot(res.pca, 
                   # Individuals
                   geom.ind = "point",
                   repel = TRUE,
                   select.var = list(contrib = 6),
                   fill.ind = as.factor(data$BrS),
                   pointshape = 21, pointsize = 3,
                   arrowsize = 1,
                   palette = "jco",
                   addEllipses = TRUE,
                   # Variables
                   alpha.var ="contrib", col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
                   
)+
  labs(fill = "Cohort", color = "Contrib", alpha = "Contrib")+
  theme(axis.text=element_text(size=18), 
        axis.title=element_text(size=18),
        legend.title = element_text(size = 18), 
        legend.text=element_text(size=18),
        plot.title = element_blank())
p1




