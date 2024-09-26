## This file accompanies the course 732G08 Multivariate Methods 

## This software comes AS IS in the hope that it will be useful WITHOUT ANY WARRANTY, 
## NOT even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
## Please understand that there may still be bugs and errors. Use it at your own risk. 
## We take no responsibility for any errors or omissions in this code or for any misfortune 
## that may befall you or others as a result of its use. Please send comments and report 
## bugs to Krzysztof Bartoszek at krzbar@protonmail.ch .


library(ContaminatedMixt)
data(wine,package="ContaminatedMixt") ## this data is also in the candisc package

library(MASS)
library(candisc)
library(multigroup)
library(RMaCzek)


modellda<-MASS::lda(Type~.,data=wine) #Type: cultivar, i.e., kind of plant, of wine
plot(modellda)
vcol<-rep("blue",nrow(wine))
vcol[which(wine$Type=="Barolo")]<-"green"
vcol[which(wine$Type=="Barbera")]<-"red"
plot(modellda,col=vcol)


TBWwine<-multigroup::TBWvariance(wine[,-1], wine$Type)
W<-TBWwine$Within.Var
B<-TBWwine$Between.Var 
invWB<-solve(W)%*%B
eigen(invWB)
eigen(invWB)$vectors[,1:2]


wine_lm<-lm(cbind(Alcohol,Malic,Ash,Alcalinity,Magnesium,Phenols,Flavanoids,Nonflavanoid,Proanthocyanins,Color,Hue,Dilution,Proline)~Type,data=wine)
wine_cda<-candisc::candisc(wine_lm,data=wine)
plot(wine_cda)


wine_cda$eigenvalues
eigen(invWB)$values

modellda$scaling
wine_cda$coeffs.raw
eig12<-eigen(invWB)$vectors[,1:2]
eig12[,1]<-eig12[,1]/(sqrt((eig12[,1])%*%W%*%eig12[,1])[1,1]) ## with this scaing we obtain the same coefficients as lda and candisc
eig12[,2]<-eig12[,2]/(sqrt((eig12[,2])%*%W%*%eig12[,2])[1,1])

#wine_cda$coeffs.raw/(wine_cda$coeffs.raw[1,1]
#modellda$scaling/(modellda$scaling[1,1]

plot(czek_matrix(wine[,-1],cluster = TRUE, cluster_type = "exact", num_cluster = 3, min.size = 2),plot_title="1:59 Barolo; 60:130 Grignolino; 131:178 Barbera")