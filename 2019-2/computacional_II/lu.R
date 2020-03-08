set.seed(123)
n <- 1000; b0 <- 10; b1 <- 0.5
x <- rnorm(n, mean = 150, sd = 15)
sigma2 <- 20
y <- b0 + b1*x + rnorm(n, mean = 0, sd = sqrt(sigma2))

## Número de amostras
r <- 1e4
## Número de elementos em cada amostra
m <- 100


library(profvis)

profvis({
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
})

library(microbenchmark)
b0.boot <- numeric(r)
b1.boot <- numeric(r)
lin<- function(x,y,b0.boot,b1.boot){
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

microbenchmark(lin(x,y=y,b0.boot,b1.boot), times = 5)

###Testando lm.fit

profvis({
  b0.boot <- numeric(r)
  b1.boot <- numeric(r)
  set.seed(123)
  for(i in 1:r){
    select <- sample(1:length(y), size = m, replace = TRUE)
    x.boot <- x[select]
    y.boot <- y[select]
    mm <- lm.fit(cbind(1,x.boot), y.boot)
    b0.boot[i] <- coef(mm)[1]
    b1.boot[i] <- coef(mm)[2]
  }
})


library(microbenchmark)
b0.boot <- numeric(r)
b1.boot <- numeric(r)
lin<- function(x,y,b0.boot,b1.boot){
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