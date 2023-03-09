library(factoextra)
library(viridis)
library(corrplot)

data<-read.csv("new_data_3.txt", sep = "\t", header = T)
data=as.data.frame(na.omit(data))
dim(data)
data=data[,c(1,2,52,56,57,92,99)]
mm<- data.matrix(data[,c(-1,-2)])

dim(mm)

res.pca <- prcomp(mm, scale = T)
fviz_pca_ind(res.pca)
p3= fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 95))+
  theme(axis.text=element_text(size=18), 
        axis.title=element_text(size=18),
        legend.title = element_text(size = 18),
        legend.text=element_text(size=18),
        plot.title = element_blank())
p3

p2=fviz_pca_var(res.pca, col.var="contrib", select.var = list(contrib = 8), repel = TRUE, arrowsize = 1)+
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
                   select.var = list(contrib = 8),
                   fill.ind = as.factor(data$BrS),
                   pointshape = 21, pointsize = 3,
                   arrowsize = 1,
                   palette = "jco",
                   addEllipses = TRUE,
                   # Variables
                   alpha.var ="contrib", col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   
)+
  labs(fill = "Type", color = "Contrib", alpha = "Contrib")+
  theme(axis.text=element_text(size=18), 
        axis.title=element_text(size=18),
        legend.title = element_text(size = 18), 
        legend.text=element_text(size=18),
        plot.title = element_blank())
p1

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 12)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 12)
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)
var <- get_pca_var(res.pca)

head(var$coord, 4)
corrplot(var$contrib, is.corr=FALSE)
corrplot(var$cos2, is.corr=FALSE)


#################  Z ####################

data<-read.csv("new_data_3z.txt", sep = "\t", header = T)
data=as.data.frame(na.omit(data))
dim(data)
data=data[,c(2,49,53,54,63,85,89,96)]
mm<- data.matrix(data[,c(-1)])

dim(mm)

res.pca <- prcomp(mm, scale = T)
fviz_pca_ind(res.pca)
p3= fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 95))+
  theme(axis.text=element_text(size=18), 
        axis.title=element_text(size=18),
        legend.title = element_text(size = 18),
        legend.text=element_text(size=18),
        plot.title = element_blank())
p3

p2=fviz_pca_var(res.pca, col.var="contrib", select.var = list(contrib = 12), repel = TRUE, arrowsize = 1)+
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
                   select.var = list(contrib = 12),
                   fill.ind = as.factor(data$BrS),
                   pointshape = 21, pointsize = 3,
                   arrowsize = 1,
                   palette = "jco",
                   addEllipses = TRUE,
                   # Variables
                   alpha.var ="contrib", col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   
)+
  labs(fill = "Type", color = "Contrib", alpha = "Contrib")+
  theme(axis.text=element_text(size=18), 
        axis.title=element_text(size=18),
        legend.title = element_text(size = 18), 
        legend.text=element_text(size=18),
        plot.title = element_blank())
p1

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 12)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 12)
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)
var <- get_pca_var(res.pca)

head(var$coord, 4)
corrplot(var$contrib, is.corr=FALSE)
corrplot(var$cos2, is.corr=FALSE)





p0=fviz_pca_biplot(res.pca, axes = c(3, 4),
                   # Individuals
                   geom.ind = "point",
                   repel = TRUE,
                   select.var = list(contrib = 12),
                   fill.ind = as.factor(data$BrS),
                   pointshape = 21, pointsize = 3,
                   arrowsize = 1,
                   palette = "jco",
                   addEllipses = TRUE,
                   # Variables
                   alpha.var ="contrib", col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   
)+
  labs(fill = "Type", color = "Contrib", alpha = "Contrib")+
  theme(axis.text=element_text(size=18), 
        axis.title=element_text(size=18),
        legend.title = element_text(size = 18), 
        legend.text=element_text(size=18),
        plot.title = element_blank())
p0
