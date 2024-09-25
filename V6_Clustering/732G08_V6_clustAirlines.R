## This file accompanies the course 732G08 Multivariate Methods 

## This software comes AS IS in the hope that it will be useful WITHOUT ANY WARRANTY, 
## NOT even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
## Please understand that there may still be bugs and errors. Use it at your own risk. 
## We take no responsibility for any errors or omissions in this code or for any misfortune 
## that may befall you or others as a result of its use. Please send comments and report 
## bugs to Krzysztof Bartoszek at krzbar@protonmail.ch .


## This is the Airline Accidents data set from https://github.com/fivethirtyeight/data/tree/master/airline-safety
## Data behind https://fivethirtyeight.com/features/should-travelers-avoid-flying-airlines-that-have-had-crashes-in-the-past/
## Source: Aviation Safety Network (http://aviation-safety.net/)
## Variables
## airline 	Airline (asterisk indicates that regional subsidiaries are included)
## avail_seat_km_per_week 	Available seat kilometers flown every week
## incidents_85_99 	Total number of incidents, 1985–1999
## fatal_accidents_85_99 	Total number of fatal accidents, 1985–1999
## fatalities_85_99 	Total number of fatalities, 1985–1999
## incidents_00_14 	Total number of incidents, 2000–2014
## fatal_accidents_00_14 	Total number of fatal accidents, 2000–2014
## fatalities_00_14 	Total number of fatalities, 2000–2014


library(ggpubr) ## for plotting kmeans clusters
library(factoextra) ## for plotting kmeans clusters

## download the data
dfAirlineAccidents<-read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/airline-safety/airline-safety.csv",header=TRUE)

## use hclust (agglomerative/hierarchical)
print("Hclust default")
plot(hclust(dist(dfAirlineAccidents[,-1])),labels=dfAirlineAccidents[,1])
readline()
plot(hclust(dist(dfAirlineAccidents[,-c(1,2)])),labels=dfAirlineAccidents[,1])
readline()
print("Hclust centroid")
plot(hclust(dist(dfAirlineAccidents[,-1]),method="centroid"),labels=dfAirlineAccidents[,1])
readline()
plot(hclust(dist(dfAirlineAccidents[,-c(1,2)]),method="centroid"),labels=dfAirlineAccidents[,1])
readline()
print("Hclust average")
plot(hclust(dist(dfAirlineAccidents[,-1]),method="average"),labels=dfAirlineAccidents[,1])
readline()
plot(hclust(dist(dfAirlineAccidents[,-c(1,2)]),method="average"),labels=dfAirlineAccidents[,1])
readline()
## ===============================


rownames(dfAirlineAccidents)<-dfAirlineAccidents[,1]
dfAirlineAccidents<-dfAirlineAccidents[,-1]

## use kmeans (divisive)

print("Kmeans default")
Kmeans_default<-kmeans(dfAirlineAccidents,2)
plot(factoextra::fviz_cluster(Kmeans_default,dfAirlineAccidents))
readline()
Kmeans_default<-kmeans(dfAirlineAccidents[,-1],2)
plot(factoextra::fviz_cluster(Kmeans_default,dfAirlineAccidents[,-1]))
readline()

print("Kmeans Lloyd")
Kmeans_Lloyd<-kmeans(dfAirlineAccidents,2,algorithm="Lloyd",iter.max=20)
plot(factoextra::fviz_cluster(Kmeans_Lloyd,dfAirlineAccidents))
readline()
Kmeans_Lloyd<-kmeans(dfAirlineAccidents[,-1],2,algorithm="Lloyd",iter.max=20)
plot(factoextra::fviz_cluster(Kmeans_Lloyd,dfAirlineAccidents[,-1]))
readline()
## the below clustering can be achieved after a couple of tries of the Kmeans Lloyd algorithm, i.e., the initial centroids are randomly chosen airlines, so need to wait to get just by chance the ones which generate this clustering
m_startpoint<-rbind(c(10,2.5,53.5,7,2.25,393.25),c(6.961538,2.153846,116.9423,3.903846,0.5384615,29.53846))
colnames(m_startpoint)<-colnames(dfAirlineAccidents[,-1])
Kmeans_Lloyd<-kmeans(dfAirlineAccidents[,-1],m_startpoint,algorithm="Lloyd",iter.max=20)
plot(factoextra::fviz_cluster(Kmeans_Lloyd,dfAirlineAccidents[,-1]))
readline()

print("Kmeans MacQueen")
Kmeans_McQ<-kmeans(dfAirlineAccidents,2,algorithm="MacQueen")
plot(factoextra::fviz_cluster(Kmeans_McQ,dfAirlineAccidents))
readline()
Kmeans_McQ<-kmeans(dfAirlineAccidents[,-1],2,algorithm="MacQueen")
plot(factoextra::fviz_cluster(Kmeans_McQ,dfAirlineAccidents[,-1]))
readline()
## ===============================

## experiment with RMaCzek
library(RMaCzek)

print("RMaCzek OLO")
RMaCzek_OLO<-czek_matrix(dfAirlineAccidents,order="OLO",cluster = TRUE, cluster_type = "exact", num_cluster = 2, min.size = 2)
plot(RMaCzek_OLO)
readline()
RMaCzek_OLO<-czek_matrix(dfAirlineAccidents[,-1],order="OLO",cluster = TRUE, cluster_type = "exact", num_cluster = 2, min.size = 2)
plot(RMaCzek_OLO)
readline()

print("RMaCzek OLO Ward")
RMaCzek_OLOWard<-czek_matrix(dfAirlineAccidents,order="OLO_ward",cluster = TRUE, cluster_type = "exact", num_cluster = 2, min.size = 2)
plot(RMaCzek_OLOWard)
readline()
RMaCzek_OLOWard<-czek_matrix(dfAirlineAccidents[,-1],order="OLO_ward",cluster = TRUE, cluster_type = "exact", num_cluster = 2, min.size = 2)
plot(RMaCzek_OLOWard)
readline()

print("RMaCzek OLO average")
RMaCzek_OLOavg<-czek_matrix(dfAirlineAccidents,order="OLO_average",cluster = TRUE, cluster_type = "exact", num_cluster = 2, min.size = 2)
plot(RMaCzek_OLOavg)
readline()
RMaCzek_OLOavg<-czek_matrix(dfAirlineAccidents[,-1],order="OLO_average",cluster = TRUE, cluster_type = "exact", num_cluster = 2, min.size = 2)
plot(RMaCzek_OLOavg)
readline()


print("RMaCzek HC")
RMaCzek_HC<-czek_matrix(dfAirlineAccidents,order="HC",cluster = TRUE, cluster_type = "exact", num_cluster = 2, min.size = 2)
plot(RMaCzek_HC)
readline()
RMaCzek_HC<-czek_matrix(dfAirlineAccidents[,-1],order="HC",cluster = TRUE, cluster_type = "exact", num_cluster = 2, min.size = 2)
plot(RMaCzek_HC)
readline()

print("RMaCzek HC Ward")
RMaCzek_HCWard<-czek_matrix(dfAirlineAccidents,order="HC_ward",cluster = TRUE, cluster_type = "exact", num_cluster = 2, min.size = 2)
plot(RMaCzek_HCWard)
readline()
RMaCzek_HCWard<-czek_matrix(dfAirlineAccidents[,-1],order="HC_ward",cluster = TRUE, cluster_type = "exact", num_cluster = 2, min.size = 2)
plot(RMaCzek_HCWard)
readline()

print("RMaCzek HC average")
RMaCzek_HCavg<-czek_matrix(dfAirlineAccidents,order="HC_average",cluster = TRUE, cluster_type = "exact", num_cluster = 2, min.size = 2)
plot(RMaCzek_HCavg)
readline()
RMaCzek_HCavg<-czek_matrix(dfAirlineAccidents[,-1],order="HC_average",cluster = TRUE, cluster_type = "exact", num_cluster = 2, min.size = 2)
plot(RMaCzek_HCavg)
readline()
## ===============================
