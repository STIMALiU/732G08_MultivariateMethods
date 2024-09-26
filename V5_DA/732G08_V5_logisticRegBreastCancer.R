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
dfCoimbra$Classification<-as.factor(dfCoimbra$Classification) ## glm binomial will not understand that 1,2 are levels
glmCoimbra<-glm(Classification~Age+BMI+Glucose+Insulin+HOMA+Leptin+Adiponectin+Resistin+MCP.1,family="binomial",data=dfCoimbra)
print(summary(glmCoimbra))


## now create a new observation
## the columns are very heterogeneous between each other and we would like something resembling the data
newdata<-apply(dfCoimbra[,-10],2,mean)
df_newdata<-as.data.frame(matrix(newdata,nrow=1))
names(df_newdata)<-colnames(dfCoimbra)[-10]
print(predict(glmCoimbra,df_newdata,type="response")) ## we need type="response" to get a probability

newdata<-rexp(9) ## this will not resemble the data
df_newdata<-as.data.frame(matrix(newdata,nrow=1))
names(df_newdata)<-colnames(dfCoimbra)[-10]
print(predict(glmCoimbra,df_newdata,type="response")) ## we need type="response" to get a probability



