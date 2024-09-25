## This file accompanies the course 732G08 Multivariate Methods 

## This software comes AS IS in the hope that it will be useful WITHOUT ANY WARRANTY, 
## NOT even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
## Please understand that there may still be bugs and errors. Use it at your own risk. 
## We take no responsibility for any errors or omissions in this code or for any misfortune 
## that may befall you or others as a result of its use. Please send comments and report 
## bugs to Krzysztof Bartoszek at krzbar@protonmail.ch .


## This is the Coimbra breast cancer data set from https://archive.ics.uci.edu/dataset/451/breast+cancer+coimbra
## M. Patricio, J. Pereira, J. Crisostomo, P. Matafome, M. Gomes, Raquel Seica, F. Caramelo. (2018)
## Using Resistin, glucose, age and BMI to predict the presence of breast cancer, BMC Cancer, Vol 18.
## Variable Information (from the above www) 
## Quantitative Attributes: 
## Age (years)
## BMI (kg/m2)
## Glucose (mg/dL)
## Insulin (microU/mL)
## HOMA
## Leptin (ng/mL)
## Adiponectin (microg/mL)
## Resistin (ng/mL)
## MCP-1(pg/dL)
## Labels (column 10):
## 1=Healthy controls 2=Patients

## In general we do not get any good clustering

library(ggpubr) ## for plotting kmeans clusters
library(factoextra) ## for plotting kmeans clusters

## download the data
dfCoimbra<-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00451/dataR2.csv",header=TRUE)

f_plotkmeans<-function(df,label_col,res_kmeans){
## plotting function taken from
## https://www.datanovia.com/en/blog/k-means-clustering-visualization-in-r-step-by-step-guide/#plot-k-means
    res.pca <- prcomp(df[, -label_col],  scale = TRUE)
    # Coordinates of individuals
    ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
    # Add clusters obtained using the K-means algorithm
    ind.coord$cluster <- factor(res_kmeans$cluster)
    # Add Species groups from the original data sett
    ind.coord$Label <- as.factor(df[,label_col])
    # Percentage of variance explained by dimensions
    eigenvalue <- round(get_eigenvalue(res.pca), 1)
    variance.percent <- eigenvalue$variance.percent
    ggscatter(
     ind.coord, x = "Dim.1", y = "Dim.2", 
     color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
      shape = "Label", size = 1.5,  legend = "right", ggtheme = theme_bw(),
      xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
      ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
    ) +
     stat_mean(aes(color = cluster), size = 4)
}


## experiment with hclust
print("Hclust default")
plot(hclust(dist(dfCoimbra[,-10])),labels=dfCoimbra[,10])
readline()
print("Hclust centroid")
plot(hclust(dist(dfCoimbra[,-10]),method="centroid"),labels=dfCoimbra[,10])
readline()
print("Hclust average")
plot(hclust(dist(dfCoimbra[,-10]),method="average"),labels=dfCoimbra[,10])
readline()
## ===============================

## experiment with kmeans
Kmeans_default<-kmeans(dfCoimbra[,-10],2)
print("Kmeans default")
print(table(Kmeans_default$cluster,dfCoimbra[,10]))
plot(f_plotkmeans(dfCoimbra,10,Kmeans_default))
readline()

Kmeans_Lloyd<-kmeans(dfCoimbra[,-10],2,algorithm="Lloyd",iter.max=20)
print("Kmeans Lloyd")
print(table(Kmeans_Lloyd$cluster,dfCoimbra[,10]))
plot(f_plotkmeans(dfCoimbra,10,Kmeans_Lloyd))
readline()

print("Kmeans MacQueen")
Kmeans_McQ<-kmeans(dfCoimbra[,-10],2,algorithm="MacQueen")
print(table(Kmeans_McQ$cluster,dfCoimbra[,10]))
plot(f_plotkmeans(dfCoimbra,10,Kmeans_McQ))
readline()

print("Kmeans starting point")
Kmeans_LabelCenter<-kmeans(dfCoimbra[,-10],rbind(apply(dfCoimbra[which(dfCoimbra[,10]==1),-10],2,mean),apply(dfCoimbra[which(dfCoimbra[,10]==2),-10],2,mean)))
print(table(Kmeans_LabelCenter$cluster,dfCoimbra[,10]))
plot(f_plotkmeans(dfCoimbra,10,Kmeans_LabelCenter))
readline()
## ===============================


## experiment with RMaCzek
library(RMaCzek)

RMaCzek_OLO<-czek_matrix(dfCoimbra[,-10],order="OLO",cluster = TRUE, cluster_type = "exact", num_cluster = 2, min.size = 2)
print("RMaCzek OLO")
print(table(attr(RMaCzek_OLO,"cluster_res"),dfCoimbra[,10]))
plot(RMaCzek_OLO)
readline()

RMaCzek_OLOWard<-czek_matrix(dfCoimbra[,-10],order="OLO_ward",cluster = TRUE, cluster_type = "exact", num_cluster = 2, min.size = 2)
print("RMaCzek OLO Ward")
print(table(attr(RMaCzek_OLOWard,"cluster_res"),dfCoimbra[,10]))
plot(RMaCzek_OLOWard)
readline()


RMaCzek_OLOavg<-czek_matrix(dfCoimbra[,-10],order="OLO_average",cluster = TRUE, cluster_type = "exact", num_cluster = 2, min.size = 2)
print("RMaCzek OLO average")
print(table(attr(RMaCzek_OLOavg,"cluster_res"),dfCoimbra[,10]))
plot(RMaCzek_OLOavg)


RMaCzek_HCWard<-czek_matrix(dfCoimbra[,-10],order="HC_ward",cluster = TRUE, cluster_type = "exact", num_cluster = 2, min.size = 2)
print("RMaCzek HC Ward")
print(table(attr(RMaCzek_HCWard,"cluster_res"),dfCoimbra[,10]))
plot(RMaCzek_HCWard)
readline()

RMaCzek_HCavg<-czek_matrix(dfCoimbra[,-10],order="HC_average",cluster = TRUE, cluster_type = "exact", num_cluster = 2, min.size = 2)
print("RMaCzek HC average")
print(table(attr(RMaCzek_HCavg,"cluster_res"),dfCoimbra[,10]))
plot(RMaCzek_HCavg)
readline()
## ===============================