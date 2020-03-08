#Y-i|theta ~ Exp(lambda-i)
#lambda-i = 
# Poisson
x <- seq(0, 1, l = 100)
b0 = log(10)
b1 = 0.5

eta = b0 +b1*x # preditor de cada lambda
plot(eta - x)
lambda = exp(eta)

#simulando

set.seed(123)
y = rpois(100, lambda = lambda)
plot(y -x)

dados = data.frame("y" = y, "x" = x)
head(dados)

#log-verossimilhança
ll <- function(theta, y, x){
  lambda = exp(theta[1] + theta[2]*x)
  output <- -sum(dpois(y, lambda = lambda, log = TRUE))
  return(output)
}

# avaliar
ll(theta = c(log(10), 0.5), y = y, x = x)
ll(theta = c(log(10), 0), y = y, x = x)

# maximizando a função
grid_b0 <- seq(0.1, 5, l = 50)
grid_b1 <- seq(-1, 1, l = 50)
grid = expand.grid(grid_b0, grid_b1)
head(grid)

#avaliando em todos os pontos do grid
ll_valor <- c()
for(i in 1:2500){
  ll_valor[i] <- ll(theta = as.numeric(grid[i, ]), y = y, x = x)
}

image(grid_b0, grid_b1, matrix(ll_valor, 50, 50))
grid$ll_valor = ll_valor
grid[which.max(grid$ll_valor), ]

#otimizando numericamente
#colocar ll para retornar o negativo da ll antes
oo <- optim(par = c(2.54, 0), fn = ll, y = y, x = x, hessian = TRUE)
#hessian são os valores das funções score
str(oo)
oo$par
oo$value
oo$convergence
oo$message
inv_Io <- solve(oo$hessian)
inv_Io
ic_Max <- oo$par + qnorm(0.975)*sqrt(diag(inv_Io)) 
ic_Min <- oo$par - qnorm(0.975)*sqrt(diag(inv_Io))
cbind(ic_Min, oo$par, ic_Max)
oo$par
#----------------

# trabalho
#observações https://www.inf.ufsc.br/~andre.zibetti/probabilidade/exponencial.html




