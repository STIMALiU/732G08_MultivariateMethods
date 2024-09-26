## This file accompanies the course 732G08 Multivariate Methods 

## This software comes AS IS in the hope that it will be useful WITHOUT ANY WARRANTY, 
## NOT even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
## Please understand that there may still be bugs and errors. Use it at your own risk. 
## We take no responsibility for any errors or omissions in this code or for any misfortune 
## that may befall you or others as a result of its use. Please send comments and report 
## bugs to Krzysztof Bartoszek at krzbar@protonmail.ch .

library(RMaCzek)
library(RCurl)
library(readr)
## description of data is at https://users.stat.ufl.edu/~winner/data/nazi.dat
## columns are Religion: 1=Protestant,2=Catholic,3=none;Cohort: 1=Empire,2=Late Empire, 3=Early Weimer, 4=Late Weimer, 5=Third Reich;Residence: 1=rural, 2=urban; Gender: 1=male,2=female;NSDAP_membership: 1=yes, 0=no;Count in category
## data is sourced from sourced from German Teachers and Nazi party membership data sourced from K.H. Jarausch and G. Arminger (1989), The German Teaching Profession and Nazi Party Membership: A Demographic Logit Model, Journal of Interdisciplinary History, Vol. 20, pp 197-225.


library(readr)
col_nums <- fwf_cols(religion = c(1,2), cohort = c(4, 5), residence = c(8,9), gender = c(12, 13), NSDAP_membership = c(16, 17), count = c(20, 23)) 

DEteachers<-as.matrix(read_fwf("https://users.stat.ufl.edu/~winner/data/nazi.dat",col_positions = col_nums))
DEteachers_logistic<-DEteachers[1:60,1:4]
DEteachers_logistic<-cbind(DEteachers_logistic,count_NSDAPmember=DEteachers[1:60,"count"],count_notNSDAPmember=DEteachers[61:120,"count"])

dfDEteachers_logistic<-as.data.frame(DEteachers_logistic)
dfDEteachers_logistic[,1]<-as.factor(dfDEteachers_logistic[,1])
dfDEteachers_logistic[,2]<-as.factor(dfDEteachers_logistic[,2])
dfDEteachers_logistic[,3]<-as.factor(dfDEteachers_logistic[,3])
dfDEteachers_logistic[,4]<-as.factor(dfDEteachers_logistic[,4])

glmDEteachers<-glm(cbind(count_NSDAPmember,count_notNSDAPmember)~religion+cohort+residence+gender,family="binomial",data=dfDEteachers_logistic)

probsDEteachers<-dfDEteachers_logistic[,1:4]
probsDEteachers<-cbind(probsDEteachers,probNSDAP=predict(glmDEteachers,probsDEteachers,type="response"))

dfDEteachers_logistic<-cbind(dfDEteachers_logistic,prop_NSDAP=dfDEteachers_logistic[,"count_NSDAPmember"]/(dfDEteachers_logistic[,"count_NSDAPmember"]+dfDEteachers_logistic[,"count_notNSDAPmember"]))
glmDEteachers_props<-glm(cbind(count_NSDAPmember,count_notNSDAPmember)~religion+cohort+residence+gender,family="binomial",data=dfDEteachers_logistic)

probsDEteachers_props<-dfDEteachers_logistic[,1:4]
probsDEteachers_props<-cbind(probsDEteachers_props,probNSDAP=predict(glmDEteachers_props,probsDEteachers,type="response"))

## results are the same between the two glm() calls

