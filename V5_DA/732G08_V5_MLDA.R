## This file accompanies the course 732G08 Multivariate Methods 

## This software comes AS IS in the hope that it will be useful WITHOUT ANY WARRANTY, 
## NOT even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
## Please understand that there may still be bugs and errors. Use it at your own risk. 
## We take no responsibility for any errors or omissions in this code or for any misfortune 
## that may befall you or others as a result of its use. Please send comments and report 
## bugs to Krzysztof Bartoszek at krzbar@protonmail.ch .

library(mvtnorm)
library(MASS)
library(candisc)
library(multigroup)
library(ContaminatedMixt)
data(wine,package="ContaminatedMixt") ## this data is also in the candisc package


## get mean vector of variables
v_mu_Barolo<-apply(wine[which(wine$Type=="Barolo"),-1,drop=FALSE],2,mean)
v_mu_Barbera<-apply(wine[which(wine$Type=="Barbera"),-1,drop=FALSE],2,mean)
v_mu_Grignolino<-apply(wine[which(wine$Type=="Grignolino"),-1,drop=FALSE],2,mean)

v_mean_wine<-matrix(v_mu_Barolo,ncol=length(v_mu_Barolo),nrow=nrow(wine),byrow=TRUE)
v_mean_wine[which(wine$Type=="Barbera"),]<-v_mu_Barbera
v_mean_wine[which(wine$Type=="Grignolino"),]<-v_mu_Grignolino

m_Sigma<-cov(wine[,-1,drop=FALSE]-v_mean_wine)

f_MLDAmvnorm<-function(v_x,l_means,m_Sigma,v_classnames){
    v_loglik<-sapply(l_means,function(v_mu,v_x,m_Sigma){
	dmvnorm(v_x,mean=v_mu,sigma=m_Sigma,log=TRUE)
    },v_x=v_x,m_Sigma=m_Sigma,simplify=TRUE)
    names(v_loglik)<-v_classnames
    MLclass<-v_classnames[which.max(v_loglik)]
    list(MLclass=MLclass,MLclass_index<-which.max(v_loglik),logliks=v_loglik)
}

v_newwine<-jitter(apply(wine[,-1],2,sample,size=1)) ## notice that we do ML under multivariate normal, but actually the measurements are positive
## the columns have very different distributions between each other, so we cannot use a homogeneous sampling model for all variables
names(v_newwine)<-colnames(wine)[-1]
l_newwine_MLDA<-f_MLDAmvnorm(v_newwine,list(v_mu_Barolo,v_mu_Barbera,v_mu_Grignolino),m_Sigma,c("Barolo","Barbera","Grignolino"))

print(l_newwine_MLDA$MLclass)
print(l_newwine_MLDA$logliks)

print("Now plot the new wine")
## ==========================================
## now we will try to plot it based on LDA



modellda<-MASS::lda(Type~.,data=wine) #Type: cultivar, i.e., kind of plant, of wine

TBWwine<-multigroup::TBWvariance(wine[,-1], wine$Type)
W<-TBWwine$Within.Var
B<-TBWwine$Between.Var 
invWB<-solve(W)%*%B
eig12<-eigen(invWB)$vectors[,1:2]
eig12[,1]<-eig12[,1]/sqrt((eig12[,1]%*%W%*%eig12[,1])[1,1]) ## with this scaling we obtain the same coefficients as with candisc
eig12[,2]<-(-1)*eig12[,2]/sqrt((eig12[,2]%*%W%*%eig12[,2])[1,1])


wine_lm<-lm(cbind(Alcohol,Malic,Ash,Alcalinity,Magnesium,Phenols,Flavanoids,Nonflavanoid,Proanthocyanins,Color,Hue,Dilution,Proline)~Type,data=wine)
wine_cda<-candisc::candisc(wine_lm,data=wine)

df_newwine<-as.data.frame(matrix(v_newwine,nrow=1))
names(df_newwine)<-names(v_newwine)

l_lda_newwine<-predict(modellda,df_newwine)
v_newwine_DApos_cand<-v_newwine%*%wine_cda$coeffs.raw
v_newwine_DApos_TBW<-Re(v_newwine%*%eig12)


print(l_lda_newwine$class)
print(l_lda_newwine$posterior)
print(rbind(v_newwine_DApos_cand,v_newwine_DApos_TBW,l_lda_newwine$x)) ## the lda coefficients are different as in ?pedict.lda "This version centres the linear discriminants so that the weighted mean (weighted by ‘prior’) of the group centroids is at the origin."


## plot the data projected into 2D
vcol<-rep("blue",nrow(wine))
vcol[which(wine$Type=="Barolo")]<-"green"
vcol[which(wine$Type=="Barbera")]<-"red"
plot(modellda,col=vcol)

## and now plot the new wine point
newwine_col<-switch(l_newwine_MLDA$MLclass,
    Barolo="green",
    Barbera="red",
    Grignolino="blue"
)

## Colour is from ML prediction
## Position and text is from LDA prediction
## notice they sometimes agree and sometimes not
points(l_lda_newwine$x,col=newwine_col,pch=19)
text(l_lda_newwine$x,label=l_lda_newwine$class,,col=newwine_col)

