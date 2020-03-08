library(utils)
library(kableExtra)
library(combinat)
library(tinytex)
library(MASS)
##trab amostragem

# Trabalho 1
## Tabela 1
x1 <- c("y1", "y2", "y3", "y4", "y5", "y6")
y1 <- c(2, 4, 5, 7, 8, 9)
df1 <- data.frame(x = x1, y = y1)
t1_1 <- knitr::kable(df1, align = 'clc', format = "markdown",
                     col.names = c("População", "Valores"))
t1_1
mp1 <- round(sum(df1$y)/6, digits = 2)

## Tabela 2
# Coluna de todas as amostras possíveis de tamanho 2
amostras2 <- combn2(y1) # Todas as amostras tamanho 2
amostras2 <- paste(amostras2[,1], amostras2[,2], sep = ",") # transformando num vetor só
# Coluna das médias amostrais 'ys' 
ys2 <- combn(y1, 2, mean) # Todas as médias das amostras de tamanho 2
# Coluna das probabilidades ys 'P(ys)' 
pys2 <- rep("1/15", 15)
# Coluna 'ys.P[ys]'
yspys2 <- round(ys2/15, digits = 2)
#Criando a tabela de n = 2
n2 <- cbind(amostras2, ys2, pys2, yspys2)
# criando a tablea para n = 2
df2 <- as.data.frame(n2)
colnames(df2) <- c("Amostra", "ys", "P(ys)", "ys.P(ys)")
# Média amostral de todas as amostras de tamanho 2 possíveis
ma2 <- sum(yspys2)
t1_2 <- knitr::kable(df2, align = 'clc', format = "markdown")
t1_2

## Tabela 3
# Coluna de todas as amostras 'Amostra'
amostras3 <- combn(y1, 3, byrow = TRUE) # Todas as amostras tamanho 2
amostras3 <- t(amostras3) # Transpondo a matriz para poder usar o paste
amostras3 <- paste(amostras3[,1], amostras3[,2], amostras3[,3], sep = ",") # transformando num vetor só
#coluna das médias amostrais 'ys' 
ys3 <- round(combn(y1, 3, mean), digits = 2) # Todas as médias das amostras de tamanho 2
#Coluna das probabilidades ys 'P(ys)' 
pys3 <- rep("1/20", 20)
#Coluna 'ys.P[ys]'
yspys3 <- round(ys3/20, digits = 2)
#Criando a tabela de n = 3
n3 <- cbind(amostras3, ys3, pys3, yspys3)
# criando a tablea para n = 3
df3 <- as.data.frame(n3)
df3
colnames(df3) <- c("Amostra", "ys", "P(ys)", "ys.P(ys)")
# Média amostral de todas as amostras de tamanho 3 possíveis
ma3 <- sum(yspys3)
t1_3 <- knitr::kable(df3, align = 'clc', format = "markdown")
t1_3

## Tabela 4
# Coluna de todas as amostras 'Amostra'
amostras4 <- combn(y1, 4) # Todas as amostras tamanho 2
amostras4 <- t(amostras4) # Transpondo a matriz para poder usar o paste
amostras4 <- paste(amostras4[,1], amostras4[,2], amostras4[,3], 
                   amostras4[,4], sep = ",") # transformando num vetor só
#coluna das médias amostrais 'ys' 
ys4 <- round(combn(y1, 4, mean), digits = 2) # Todas as médias das amostras de tamanho 2
#Coluna das probabilidades ys 'P(ys)' 
pys4 <- rep("1/15", 15)
#Coluna 'ys.P[ys]'
yspys4 <- round(ys4/15, digits = 2)
#Criando a tabela de n = 4
n4 <- cbind(amostras4, ys4, pys4, yspys4)
# criando a tablea para n = 4
df4 <- as.data.frame(n4)
colnames(df4) <- c("Amostra", "ys", "P(ys)", "ys.P(ys)")
# Média amostral de todas as amostras de tamanho 4 possíveis
ma4 <- sum(yspys4)
t1_4 <- knitr::kable(df4, align = 'clc', format = "markdown")
t1_4

# Trabalho 2
## n = 2
f21 <- ((6-2)/6)*(6.967/2)
f21

## Trabalho 3
# n = 2
# s^2
amts2 <- combn2(y1)
s22 <- c()
for(i in 1:15){
  s22[i] <- round(((amts2[i, 1]-ys2[i])^2 + 
                     (amts2[i, 2]-ys2[i])^2)/(2-1), digits = 2)
  
}
s22
#s^2P(ys)
s22p <- round(s22/15, digits = 2)
s22p
round(sum(s22p), digits = 3)
n23 <- cbind(amostras2, s22, s22p)
n23 <- as.data.frame(n23)
colnames(n23) <- c("Amostra", "s²", "s²P(ys)")
t2_2 <- knitr::kable(n23, align = 'cll', format = "markdown")
t2_2
# n = 3
# s^2
amts3 <- combn(y1, 3, byrow = TRUE)
amts3 <- cbind(amts3[1,], amts3[2, ], amts3[3, ])
s23 <- c()
for(i in 1:20){
  s23[i] <- round(((amts3[i, 1]-ys3[i])^2 + (amts3[i, 2]-ys3[i])^2 +
               (amts3[i, 3]-ys3[i])^2)/(3-1) , digits = 2)
  
}
s23

#s^2P(ys)
s23p <- round(s23/20, digits = 2)
s23p
round(sum(s23p), digits = 3)
n33 <- cbind(amostras3, s23, s23p)
n33 <- as.data.frame(n33)
colnames(n33) <- c("Amostra", "s²", "s²P(ys)")
t2_3 <- knitr::kable(n33, align = 'cll', format = "markdown")
t2_3
# n = 4
# s^2
amts4 <- combn(y1, 4, byrow = TRUE)
amts4 <- cbind(amts4[1,], amts4[2, ], amts4[3, ], amts4[4, ])
s24 <- c()
for(i in 1:15){
  s24[i] <- round(((amts4[i, 1]-ys4[i])^2 + (amts4[i, 2]-ys4[i])^2 +
               (amts4[i, 3]-ys4[i])^2 + 
                 (amts4[i, 4]-ys4[i])^2)/(4-1), digits = 2)
}
s24
#s^2P(ys)
s24p <- round(s24/15, digits = 2)
s24p
round(sum(s24p), digits = 3)
n34 <- cbind(amostras4, s24, s24p)
n34 <- as.data.frame(n34)
colnames(n34) <- c("Amostra", "s²", "s²P(ys)")
t2_4 <- knitr::kable(n34, align = 'cll', format = "markdown")
t2_4

## Trabalho 5
# n = 10
x5 <- c(96, 100, 89, 90, 95, 94, 87, 104, 85, 84)
l <- c(1, 2, 2, 3, 3, 4, 4, 5, 5, 6)
c <- c(11, 2, 13, 4, 15, 6, 17, 8, 19, 10)
t5 <- cbind(l, c, x5)
t5 <- as.data.frame(t5)
colnames(t5) <- c("Linha", "Coluna", "QI")
t5_1 <- knitr::kable(t5, align = 'lll', format = "markdown")
t5_1
round(mean(t5$QI), digits = 2)
round(sd(t5$QI), digits = 2)
# Recalc com n = 23
x52 <- c(96, 100, 89, 90, 95, 94, 87, 104, 85, 84,
         89, 89, 77, 97, 91, 99, 87, 83, 100, 80,
         90, 88, 88)
l2 <- c(1, 2, 2, 3, 3, 4, 4, 5, 5, 6,
        9, 9, 12, 12, 15, 29, 38, 28, 14, 42,
        47, 16, 34)
c2 <- c(11, 2, 13, 4, 15, 6, 17, 8, 19, 10,
        10, 13, 13, 15, 15, 1, 13, 15, 20, 15, 
        12, 9, 15)
t52 <- cbind(l2, c2, x52)
t52 <- as.data.frame(t52)
colnames(t52) <- c("Linha", "Coluna", "QI")
t5_2 <- knitr::kable(t52, align = 'lll', format = "markdown")
t5_2
round(mean(t52$QI), digits = 2)
round(sd(t52$QI), digits = 2)

## Trabalho 6
fre <- c(23, 4,  1, 1, 1, 2, 1, 1, 2, 2, 1, 1, 1, 1, 1,
                3, 2, 1, 1)
ass <- c(42, 41, 36, 32, 29, 27, 23, 19, 16, 15, 14, 11,
         10, 9, 7, 6, 5, 4, 3)
pet <- cbind(ass, fre)
pet <- as.data.frame(pet)
colnames(pet) <- c("Nr de Assinaturas", "Frequência")
est <- knitr::kable(pet, align = 'cc', format = "markdown")
est
round(mean(t52$QI), digits = 2)
round(sd(t52$QI), digits = 2)

## Exerecício 8
fa = seq(1, 20)
ar = c(24, 19, 27, 26, 27, 17, 15, 7, 13, 22, 8, 12, 13, 11, 5,
       5, 27, 23, 31, 12)
arp = c(12.5, 7.9, 11.1, 10.9, 11.2, 7, 7.9, 3.3, 4.1, 8.1, 3.3, 5.8,
        4.4, 3.7, 2.1, 1.7, 12.3, 3.5, 12.3, 4.4)
t8 <- cbind(fa, ar, arp)
t8 <- as.data.frame(t8)
colnames(t8) <- c("Fazenda", "Área Total(xi)", "Área Plantada(yi)")
t8_1 <- knitr::kable(t8, align = 'ccc', format = "markdown")
t8_1
