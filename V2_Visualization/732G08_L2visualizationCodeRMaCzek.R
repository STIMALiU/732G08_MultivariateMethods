## This file accompanies the course 732G08 Multivariate Methods 

## This software comes AS IS in the hope that it will be useful WITHOUT ANY WARRANTY, 
## NOT even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
## Please understand that there may still be bugs and errors. Use it at your own risk. 
## We take no responsibility for any errors or omissions in this code or for any misfortune 
## that may befall you or others as a result of its use. Please send comments and report 
## bugs to Krzysztof Bartoszek at krzbar@protonmail.ch .

library(RMaCzek)
## code from RMaCzek's manual
## https://cran.r-project.org/web/packages/RMaCzek/
# Set data ####
x<-czek_matrix(mtcars)
# Standard plot ############
plot(x)
plot.czek_matrix(x)
