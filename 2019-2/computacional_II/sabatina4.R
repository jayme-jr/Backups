## EX 1
## Obtendo 10000 valores de uma U(0, 0.5) 
m <- 10000
x <- runif(m, min = 0, max = .5)
##g(x)
gx <- exp(-x)
##média * (b-a)
m.gx <- (sum(gx)/m)*(.5-0)
##solução analítica
e.fx <- 1 - exp(-.5) 
## Variancia
v.gx <- sum((gx - m.gx)^2)/m^2
## Erro padrão
se.gx <- sqrt(v.gx)

## Usando exp(0.5) com amostragem por importância
# y <- seq(0, .5, .01)
# fi <- exp(-y)
y <- rexp(m, .5)
g <- function(y){
  exp(-y)
}
fg <- g(y) / exp(-y)
m.gx[2] <- mean(fg)
se.gx[2] <- sd(fg)/sqrt(m)
rbind(m.gx, se.gx)

# resp
#a, c, d, e (V)

## EX 3
# Metropolis-Hastings
# distribuição proposta N(0,1) para gerar uma cauchy(0,1)
## Define funções
# Cauchy(0,1)
set.seed(123)
f <- function(x) {
  dcauchy(x)
}
# Normal(0,1)
g <- function(x, mu){
  dnorm(x, mean = mu)
}
## Mostrando graficamente as densidades
curve(f(x), from = -20, to = 20, lwd = 2)
curve(g(x, 0), from = -20, to = 20, add = TRUE, col = 2)
# são pŕoximas

N <- 100000
x <- numeric(N)
x[1] <- rnorm(1)
k <- 0 # para contar quantos foram aceitos
for (i in 2:N) {
  y <- rnorm(1)
  num <- f(y) * g(x[i - 1], mu = y)
  den <- f(x[i - 1]) * g(y, mu = x[i - 1])
  alpha <- num/den
  u <- runif(1)
  if (u <= alpha) {
    x[i] <- y
  } else {
    x[i] <- x[i - 1]
    k <- k + 1     # contagem doa aceitos
  }
}
## Taxa de aceitação
k/N
## Histograma da distribuição e correlação entre as observações
par(mfrow = c(1, 2))
hist(x, freq = FALSE)
ind <- seq(min(x), max(x), length.out = 100)
lines(ind, f(ind), col = 2)
acf(x)
## Compara acumulada empírica com teórica
## Acumulada teórica da Rayleigh
Fx <- function(x) {
  pcauchy(x)
}
plot(ecdf(x))
curve(Fx(x), add = TRUE, col = 2)

## EX 2
#(V) a, b, d, e, h, i, j 