install.packages("stats4", dep = TRUE)
install.packages("sp", dep = TRUE)
install.packages("classInt", dep = TRUE)
install.packages("RColorBrewer", dep = TRUE)

library("stats4")
library("sp")
library("classInt")
library("RColorBrewer")

require(sp) #pacote para analise de pontos
require(classInt) #pacote para criar intervalos de classe
require(RColorBrewer) #pacote para paleta de cores

n=100
xc = round(runif(n), 2)
yc = round(runif(n), 2)
xy = cbind(xc, yc)
xy

xy.sp = SpatialPoints(xy)
xy.sp

#Plotar pontos
plot(xy.sp, pch = 2)

##adicionar eixos
plot(xy.sp, pch = 2,axes = TRUE)

xy.cc = coordinates(xy.sp)
dim(xy.cc)
bbox(xy.sp)
dimensions(xy.sp)
