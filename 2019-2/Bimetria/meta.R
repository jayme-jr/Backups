library(tidyverse)
install.packages("devtools")
devtools::install_github("MathiasHarrer/dmetar")
library(meta)
# setwd("C:/Users/luciana/Desktop/meta")
data<-read.csv("ovinos.csv", header=T, sep = ";", dec= ",")
data2 <- data %>% 
  mutate(macroreg = factor(macroreg))
  
  
mtprop <- metaprop(event = positivos, n=total, studlab=referencia, byvar=macroreg, data=data)
mtprop2 <- metaprop(event = positivos, n=total, studlab=referencia, byvar=metodo, data=data)

mtprop
mtprop2
x11()
forest(mtprop)
