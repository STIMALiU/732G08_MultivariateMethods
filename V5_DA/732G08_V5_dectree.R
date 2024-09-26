## This file accompanies the course 732G08 Multivariate Methods 

## This software comes AS IS in the hope that it will be useful WITHOUT ANY WARRANTY, 
## NOT even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
## Please understand that there may still be bugs and errors. Use it at your own risk. 
## We take no responsibility for any errors or omissions in this code or for any misfortune 
## that may befall you or others as a result of its use. Please send comments and report 
## bugs to Krzysztof Bartoszek at krzbar@protonmail.ch .

library(rpart)
library(rpart.plot)
dfabalone<-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data",header=FALSE)
colnames(dfabalone)<-c("sex","length","diameter","height","whole_weight","shucked_weight","viscera_weight","shell_weight","rings")
## age in years is rings+1.5

rpart.plot(treeabalone) ## the nodes of the tree predict the number of rings, rounded to integer in presentation
treeablone ## displaying the tree gives the actual predicted number of rings
