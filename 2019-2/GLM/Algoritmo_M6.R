### Ajuste de um GLM: Distribuição de Bernoulli

y <- c(0,0,0,0,1,0,1,1,1,1) ### valores para a variável resposta.
x <- 1:10 ### valores para a variável explicativa. 

plot(x, y, pch=20)
X <- matrix(c(rep(1,10),x),nrow=10) ### Matriz do modelo;
### Matriz do modelo

#################################################################
### Passo 1

mu <- c(0.1,0.1,0.1,0.1,0.9,0.1,0.9,0.9,0.9,0.9) 
### Chute inicial

z <- log(mu/(1-mu)) + (y-mu)/(mu*(1-mu))
z
### Variável dependente ajustada

w <- (mu*(1-mu))
w
### Pesos

beta <- lm(z ~ x, weights = w)$coefficients
beta
### betas para o próximo passo.

eta <- X %*% beta
eta
### preditor linear para o próximo passo.

mu <- exp(eta)/(exp(eta)+1)
mu
### mu para o próximo passo. 


#################################################################
### Passo 2 - atualizamos os valores de z e w com os resultados 
# do passo 1

z <- log(mu/(1-mu)) + (y-mu)/(mu*(1-mu))
z
### Variável dependente ajustada

w <- (mu*(1-mu))
w
### Pesos

beta <- lm(z ~ x, weights = w)$coefficients
beta
### betas para o próximo passo.

eta <- X %*% beta
eta
### preditor linear para o próximo passo.

mu <- exp(eta)/(exp(eta)+1)
mu
### mu para o próximo passo. 



#################################################################
### Passo 3 - atualizamos os valores de z e w com os resultados 
# do passo 2

z <- log(mu/(1-mu)) + (y-mu)/(mu*(1-mu))
z
### Variável dependente ajustada

w <- (mu*(1-mu))
w
### Pesos

beta <- lm(z ~ x, weights = w)$coefficients
beta
### betas para o próximo passo.

eta <- X %*% beta
eta
### preditor linear para o próximo passo.

mu <- exp(eta)/(exp(eta)+1)
mu
### mu para o próximo passo. 



#################################################################
### Passo 4 - atualizamos os valores de z e w com os resultados 
# do passo 3

z <- log(mu/(1-mu)) + (y-mu)/(mu*(1-mu))
z
### Variável dependente ajustada

w <- (mu*(1-mu))
w
### Pesos

beta <- lm(z ~ x, weights = w)$coefficients
beta
### betas para o próximo passo.

eta <- X %*% beta
eta
### preditor linear para o próximo passo.

mu <- exp(eta)/(exp(eta)+1)
mu
### mu para o próximo passo. 
### Agora repetimos esse passo atualizando o valor de mu.


###############################################################
### Agora, vamos repetir o processo para 10 passos

beta <- matrix(0, nrow=10, ncol=2)
### Matriz que vai armazenar as estimativas.

epsilon <- numeric()
### Vetor que vai armazenar as diferenças entre os resultados de
### simulações sucessivas.

for(i in 1:10){
     if(i==1) {mu <- c(0.1,0.1,0.1,0.1,0.9,0.1,0.9,0.9,0.9,0.9) 
     ### Se for a primeira iteração, então usamos nossos chutes iniciais.
     eta <- log(mu/(1-mu))}
     
     if (i!=1) {
          eta <- X %*% beta[i-1,] 
          mu <- exp(eta)/(exp(eta)+1) 
     }
     z <- log(mu/(1-mu)) + (y-mu)/(mu*(1-mu))
     w <- (mu*(1-mu))
     beta[i,] <- lm(z ~ x, weights = w)$coefficients
     if(i>1) epsilon[i-1] <- sum(((beta[i,]-beta[i-1,])/beta[i-1,])**2)
}

###############################################################
###############################################################
###############################################################
### Agora, vamos ajustar o modelo declarando a log-verossimilhança a um otimizador do R.

require(bbmle)

logvero <- function(b0, b1)
     -sum(dbinom(y, 1, exp(b0 + b1 * x)/ (1+exp(b0 + b1 * x)), log=T))
### logvero armazena a função de log verossimilhança (-)

### Precisamos de valores iniciais para beta0 e beta1.
### Vamos chutar valores iniciais para mu e fazer uma regressão linear
### simples de ln(mu/(1-mu)) ~ x

mu <- c(0.1,0.1,0.1,0.1,0.9,0.1,0.9,0.9,0.9,0.9)
ajuste <- lm(log((mu)/(1-mu)) ~ x) 
### Regressando g(mu) em função de x para obter valores iniciais para b0 e b1.

est2 <- mle2(logvero,start=list(b0=-3.23, b1=0.13))
est2


########################################################################
### Gráfico

logvero2 <- function(b0, b1,data){
    l <- sum(dbinom(data$y, 1, exp(b0 + b1 * data$x)/ (1+exp(b0 + b1 * data$x)), log=T))
    return(l)
}

data <- data.frame(x,y)
f <- Vectorize(logvero2,vectorize.args=c("b0","b1"))
b0 <- seq(-15, 0, length.out = 50)
b1 <- seq(0.5, 2.5, length.out = 50)
z <- outer(b0, b1, FUN = f, data)
zlim    <- range(z[!is.na(z)])
palette <- rev(rainbow(20))
colors  <- palette[19*(z-zlim[1])/diff(zlim) + 1] 
persp3d(b0, b1, z, col=colors)

#########################################################################


### Finalmente, usemos a função glm.

ajuste <- glm(y ~ x, family = binomial(link = "logit"))
names(ajuste)
coef(ajuste) 
### Estimativas dos coeficientes do modelo.
fitted(ajuste) 
### Probabilidades ajustadas pelo modelo para cada valor de x na amostra.
predict(ajuste, newdata = data.frame(x = c(3.5, 5.5, 7.5))) 
### Estimativas (na escala do preditor) para x=3.5; x=5.5 e x=7.5.
predict(ajuste, newdata = data.frame(x = c(3.5, 5.5, 7.5)),type='response') 
### Estimativas (na escala da resposta - probabilidades estimadas) para x=3.5; x=5.5 e x=7.5.
summary(ajuste) 
### Resumo do modelo ajustado contendo, dentre outras coisas, as estimativas 
### dos betas e os correspondnetes erros padrões.
vcov(ajuste) ### Matriz de variâncias e covariâncias estimada para \hat{\beta}.


####################################################################
### Vamos acrescentar ao gráfico o modelo ajustado. Antes, começamos
### tentando o ajuste de um modelo linear.

plot(x, y, pch=20, ylim = c(-0.2, 1.2), yaxt = 'n')
axis(2, seq(0, 1, 0.2), las = 1)
abline(h = 0, lty = 2)
abline(h = 1, lty = 2)

ajuste_lin <- lm(y ~ x)
abline(coefficients(ajuste_lin), col='blue', lwd = 2)

### Agora para o glm:
x.grid <- seq(1, 10, length.out = 100)
lines(x.grid, predict(ajuste, newdata = data.frame(x = x.grid), 
                      type = 'response'), col = 'red', lwd = 2)
