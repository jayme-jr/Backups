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