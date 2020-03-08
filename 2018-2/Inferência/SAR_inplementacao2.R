#instalando pacotes necessários
install.packages("spdep")
install.packages("mvtnorm")
require(spdep)
require(mvtnorm)


# Criando a estrutura espacial

# Lista de vizinhos
nb <- cell2nb(nrow = 10, ncol = 10)

# Matriz de vizinhanca
matriz = cell2nb(nrow=10,ncol=10,type="rook")
W = nb2mat(matriz)

#Demais variaveis
Sigma <- function(rho, sigma2, W) {
  I <- diag(rep(1,ncol(W)))
  B <- rho*W
  p1 <- solve( (I - B) )
  p2 <- solve( (I - t(B)) )
  out <- sigma2*(p1%*%p2)
  return(out)
}

Sigma2 <- as.matrix(Sigma(rho = 0.5, sigma2 = 1, W = W))
x1 <- seq(-1,1, l = 100)
mu <- 2 + 3*x1
Y <- as.numeric(rmvnorm(1, mean = mu, sigma = Sigma2))
plot(Y ~ x1)

#Verossimilhanca
L <- function(Sigma,Y) {
  p <- length(Y)
  out <- prod( ((2*pi)^(-p/2)) * (1/determinant(Sigma,logarithm=T)) * 
                 exp((-1/2)*Y%*%solve(Sigma)%*%t(Y)) )
  return(out)
  }

#Log-verossimilhanca
lv <- function(par, W, Y, x1) {
  beta0 <- par[1]
  beta1 <- par[2]
  rho <- par[3]
  sigma2 <- par[4]
  p <- length(Y)
  Sigma2 <- as.matrix(Sigma(rho = rho, sigma2 = sigma2, W = W))
  mu <- beta0 + beta1*x1
  out <- dmvnorm(x = Y, mean = mu, sigma = Sigma2, log = TRUE)
  return(as.numeric(out))
}

## Avaliando a log-Verossimilhança em um ponto
lv(par = c(2, 3, 0.5, 1), W = W, Y = Y, x1 = x1)

#Maximizando a log-verossimilhanca
est <- optim(par = c(0,0, 0, 1), fn = lv, W = W, Y = Y, x1 = x1, 
             method = "L-BFGS-B",
             lower = c(-Inf, -Inf, -0.99, 0.0001), upper = c(Inf, Inf, 0.99, Inf),
             control = list(fnscale = -1), hessian = T)
est

#Estimativas obtidas
Beta0_hat <- est$par[1]
Beta1_hat <- est$par[2]
rho_hat <- est$par[3]
sigma2_hat <- est$par[4]

#Matriz de informação observada
I_o <- (-est$hessian)
I_o

#Variâncias dos estimadores
V <- sqrt(diag(solve(I_o)))
V

#Teste de hipotese Wald
#H0: rho = 0, H1: rho != 0
rho_0 <- 0
Zn <- (rho_hat - rho_0)/(V[3])
Zn

#Intervalo com 95% de confiança
inf <- rho_hat - (1.96*V[3])
sup <- rho_hat + (1.96*V[3])
IC <- c(inf,sup)
IC

