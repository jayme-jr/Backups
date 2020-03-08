#1 
## verdadeiras

# a. Geradores de NA são usados em aplicações como jogos de azar, simulação computacional, 
# criptografia, amostragem estatística e outras áreas onde a produção de resultados 
# imprevisíveis é desejado.

# c. GNA feitas por algoritmos de computador são chamados de geradores pseudo-aleatórios 
# porque os números podem ser previstos por técnicas especilizadas de reconhecimento de padrões.

# e. Os dispositivos físicos para GNA geram números imprevisíveis e com sequência não correlacionada, 
# porém, pode não haver uma distribuição de probabilidades correta para os mesmos.

# g. Algoritmos computacionais podem produzir uma aparente longa sequência de números aleatórios mas 
# que são de fato determinados por um valor inicial (semente) quando usado no algorítmo.

# h. Uma vantagem dos algorítmos de GNA é que existe reprodutibidade uma vez que o algorítmo 
# é determinístico.

# i. Uma característica interessante de verdadeiros GNA é que eles produzem uma sequência não 
# determinística e aperiódica de valores.

#2
rcl <- function(n, x0, m, unit = TRUE) {
  x <- integer(n + 1)
  x[1] <- x0
  for(i in 2:length(x)) {
    x[i] <- ((2^16 + 3) * x[i - 1]) %% m
  }
  if(unit) x <- x/m
  return(x[-1])
}
set.seed(10)
rcl(5, 1, 2^23)

#3
set
randd <- function(x, px, u) {
  x <- 1
  Fx <- px[x]
  while(u > Fx) {
    x <- x + 1
    Fx <- Fx + px[x]
  }
  return(x)
}
X <- 1:6
px <- c(0.15, 0.12, 0.21, 0.12, 0.20, 0.20)
tt <- c(0.188928285, 0.613769950, 0.962571654, 0.001957438, 0.424364652)
for(i in 1:5){
  x <- randd(X, px, u = tt[i])
  print(paste0(x))
  }

#4
F_inv_lamax <- function(lambda, k, u){
  x <- round((exp((log(1 - u) / -k)) - 1) / lambda, digits = 3)
  return(x)
}
lambda <- 0.6
k <- 2
u <- c(0.39, 0.13, 0.49, 0.79, 0.41)
F_inv_lamax(lambda, k, u)

#5
## Triangular com a = -1, c = 0, b = -1.
f <- function(x) {
  0 + ifelse(x > 0, 1 - x, x + 1) * (x > -1) * (x < 1)
}
## Parabólica em [-1, 1]
g <- function(x) {3/4 * (1 - x^2) * (x >= -1) * (x <= 1)}
M <- 4/3
x <- c(-0.34, -0.59, 0.27, 0.19, 0.13, -0.62, -0.57, 0.88, 0.95, -0.04) 
u <- c(0.22, 0.48, 0.97, 0.97, 0.63, 0.53, 0.74, 0.57, 0.16, 0.98)

##f(x)≤(Mg(x))
r <- f(x)/(M * g(x))
for(i in 1:10){
  x <- x
if (u[i] < r[i]) {
  print(paste0("É aceito o valor canditado y= ", x[i], "com  u= ", u[i]))
} else {
  print(paste0("É rejeitado o valor canditado y= ", x[i], "com  u= ", u[i]))
}}

#6
set.seed(7, normal.kind = "Box-Muller")
bm <- round(rnorm(10), digits = 3)
sum(bm)
# a. Gere 10 valores de uma N(0,1) pelo método de Box-Muller, usando a função rnorm(). 
# Use como semente o número 7. Arredonde os 10 valores para 3 casas decimais. 
# A soma dos 10 valores arredondados é 7.006.(V)
set.seed(1, normal.kind = "Inversion")

# b. Quando uma VA é gerada por convolução, sabemos que ela é o resultado da soma de outras
# VAs independentes e identicamente distribuídas.(V)

jogar_dados <- function(n){
  x <- sample(1:6, n, replace = TRUE)
  for(i in 1:n){
    print(paste0(i,"° dado = ",x[i]))
  }
}
jogar_dados(200)
# c. Se U∼U(0,1), então X=−3logu é uma variável aleatória Exp(3).(F)

# d. Se U∼U(0,1), então X=u1−u é uma variável aleatória Logistica(0,1).(F)

# e. Para gerar valores de uma N(0,1), o método de coordenadas polares é mais
# eficiente do que o método de Box-Muller.(V)