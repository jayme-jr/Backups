library(tidyverse)
library(microbenchmark)
library(profvis)
### Sabatina 2

##1
#a. Se x <- c("y", "u"), então letters[x] retorna "y" "u"(F)
x <- c("y", "u")
letters[x]
#b. Ao indexar um data frame com [ usando o argumento drop = FALSE, 
# a classe do objeto resultante será um data frame.(V)
class(gauss[1, , drop = FALSE])
#c. Se m é uma matriz, então m <- 0 fará com que todos os elementos da matriz sejam 
# iguais a zero.(F)
m <- as.matrix(gauss)
class(m)
m <- 0
#d. Na indexação de um vetor, se o índice utilizado for um número não inteiro, 
# a seleção dos elementos não poderá ser realizada(F)
gauss$y[2.5]
#e. Considere que x é uma lista com três elementos. 
# O resultado de x[c(1, 3)] será também uma lista.(V)
z <- as.list(gauss)
class(z)
class(z[c(1, 3)])

##2
pib <- read.table("http://leg.ufpr.br/~fernandomayer/data/pib_gapminder.csv")
pib_g <- pib %>% 
  separate(V2, c("","ano", "pop", "continente", "expVida", "pibPercap"), ",") %>% 
  select(V1, ano, pop, continente, expVida, pibPercap)
pib_g <- pib_g[-1,]
#a. Apenas 3 países da Oceania estão presentes na base de dados.(F)
pib_g %>% filter(continente == "\"Oceania\"") %>% select(V1) %>% unique
#b. A população do Sri Lanka em 1952 era de 7982342 habitantes.(V)
pib_g %>% filter(V1 == "Sri Lanka") %>% filter(ano == "1952") %>% select(pop)
#c. Os dados do Marrocos não constam nessa base de dados.(V)
pib_g %>% filter(V1 == "Marrocos")
#d. Consideranto todos os anos disponíveis na base de dados, 
# a média do PIB per capita do Brasil foi de 5829.317.(V)
x <- pib_g %>% filter(V1 == "Brazil") %>% mutate(pibPercap = as.numeric(pibPercap)) %>% 
  select(pibPercap)
mean(x$pibPercap)
#e. O número de informações por continente é o seguinte: África (624), Américas (300), 
# Ásia (396), Europa (360), Oceania (24).(V)
pib_g %>% count(continente)
#f. A média do PIB per capita dos 4 países que compõem o Mercosul atualmente foi de 6281.(F)
mercosul <- c("Brazil", "Paraguay", "Uruguay", "Argentina")
m <- pib_g %>% filter(V1 %in% mercosul) %>% mutate(pibPercap = as.numeric(pibPercap)) %>% 
  select(pibPercap)
mean(x$pibPercap)
#g. A média do número de habitantes no continente europeu nos últimos 3 anos da base de dados 
# (1997, 2002, 2007) foi de 19258517.(F)
eu1 <- pib_g %>% filter(continente == "\"Europe\"") %>% filter(ano == "1997")
eu2 <- pib_g %>% filter(continente == "\"Europe\"") %>% filter(ano == "2002")
eu3 <- pib_g %>% filter(continente == "\"Europe\"") %>% filter(ano == "2007")
mean(sum(as.numeric(eu1$pop))+sum(as.numeric(eu2$pop))+as.numeric(eu3$pop))
#h. Apenas 2 países possuem expectativa de vida acima de 82 anos.(V)
pib_g %>% mutate(expVida = as.numeric(expVida)) %>% filter(expVida > 82) %>% 
  select(V1) %>% unique
#i. A expectativa de vida no Brasil em 2007 era de 72.39 anos.(V)
pib_g %>% mutate(expVida = as.numeric(expVida)) %>% filter(V1 == "Brazil") %>% 
  filter(ano == "2007") %>% select(expVida)
#j. Os dados correspondem a uma série temporal de 56 anos.(F)
a <- pib_g %>% mutate(ano = as.numeric(ano)) %>% select(ano)
max(a)-min(a)

##3
x <- 1:100
y <- numeric(length(x))
for(i in seq_along(x)) {
  if (x[i] %% 5 == 0) {
    y[i] <- x[i]/5
  } else {
    if(x[i] %% 2 == 0) {
      y[i] <- x[i]/2
    } else {
      y[i] <- x[i]
    }
  }
}
#a. O último valor de y é 100/5.(V)
y[100]
#b. Não é possível realizar todas estas operações de forma vetorizada 
# (por exemplo, usando ifelse()).(F)
y <- numeric(length(x))
for(i in seq_along(x)) {
  ifelse(x[i] %% 5 == 0, y[i] <- x[i]/5, 
         ifelse(x[i] %% 2 == 0, y[i] <- x[i]/2, y[i] <- x[i]))

}
#c. Se o resto da divisão de x por 2 ou por 5 não for zero, então y será igual a x.(V)

#d. Se o valor em x dividido por 2 for igual a zero, então y será zero.(F)

#e. Se o valor em x for divisível por 5 com resto zero, então y armazenará esse valor 
# dividido por 5.(V)

##4
## Simula do modelo
set.seed(123)
n <- 1000; b0 <- 10; b1 <- 0.5
x <- rnorm(n, mean = 150, sd = 15)
sigma2 <- 20
y <- b0 + b1*x + rnorm(n, mean = 0, sd = sqrt(sigma2))

## Número de amostras
r <- 1e4
## Número de elementos em cada amostra
m <- 100

##----------------------------------------------------------------------
## Bootstrap
# verificando com profvis
b0.boot <- numeric(r)
b1.boot <- numeric(r)
set.seed(123)
profvis({for(i in 1:r){
  select <- sample(1:length(y), size = m, replace = TRUE)
  x.boot <- x[select]
  y.boot <- y[select]
  mm <- lm(y.boot ~ x.boot)
  b0.boot[i] <- coef(mm)[1]
  b1.boot[i] <- coef(mm)[2]
}})
## Após verificação com profvis(), ficou claro que o passo que levou mais tempo foi 
## o lm(y.boot ~ x.boot), levando 5050ms
##----------------------------------------------------------------------
## Bootstrap
set.seed(123)
# Criando função para microbenchmark
teste <- function(x, y, b0.boot, b1.boot){
  for(i in 1:r){
  select <- sample(1:length(y), size = m, replace = TRUE)
  x.boot <- x[select]
  y.boot <- y[select]
  mm <- lm(y.boot ~ x.boot)
  b0.boot[i] <- coef(mm)[1]
  b1.boot[i] <- coef(mm)[2]
  }
  return(list(mm, head(b0.boot), head(b1.boot)))
}
teste(x, y, b0.boot, b1.boot)
m1 <- microbenchmark(teste(x, y, b0.boot, b1.boot), times = 5)
m1

## Para diminuir o tempo de execução do mm <- lm(y.boot ~ x.boot), 
## usarei lm.fit()

## verificando a auteração com profvis
b0.boot <- numeric(r)
b1.boot <- numeric(r)
set.seed(123)
profvis({for(i in 1:r){
  select <- sample(1:length(y), size = m, replace = TRUE)
  x.boot <- x[select]
  y.boot <- y[select]
  mm <- lm.fit(cbind(1,x.boot), y.boot)
  b0.boot[i] <- coef(mm)[1]
  b1.boot[i] <- coef(mm)[2]
}})
## Com a utilização do lm.fit() caiu para 310ms.
##----------------------------------------------------------------------
## Criando função para o microbenchmark com lm.fit()

teste2 <- function(x, y, b0.boot, b1.boot){
  for(i in 1:r){
    select <- sample(1:length(y), size = m, replace = TRUE)
    x.boot <- x[select]
    y.boot <- y[select]
    mm <- lm.fit(cbind(1,x.boot), y.boot)
    b0.boot[i] <- coef(mm)[1]
    b1.boot[i] <- coef(mm)[2]
  }
  return(list(mm, head(b0.boot), head(b1.boot)))
}
teste2(x, y, b0.boot, b1.boot)
m2 <- microbenchmark(teste(x, y, b0.boot, b1.boot), times = 5)
m2

## Comparando os resultados do microbenchmark
m1;m2
## Fica claro que com lm.fit o código ficou mais rápido.
## Simula do modelo
set.seed(123)
n <- 1000; b0 <- 10; b1 <- 0.5
x <- rnorm(n, mean = 150, sd = 15)
sigma2 <- 20
y <- b0 + b1*x + rnorm(n, mean = 0, sd = sqrt(sigma2))

## Número de amostras
r <- 1e4
## Número de elementos em cada amostra
m <- 100

##----------------------------------------------------------------------
## Bootstrap
b0.boot <- numeric(r)
b1.boot <- numeric(r)
set.seed(123)
for(i in 1:r){
  select <- sample(1:length(y), size = m, replace = TRUE)
  x.boot <- x[select]
  y.boot <- y[select]
  mm <- lm(y.boot ~ x.boot)
  b0.boot[i] <- coef(mm)[1]
  b1.boot[i] <- coef(mm)[2]
}
