### Este exemplo tem como objetivo esclarecer o significado histórico do
### termo regressão, baseado em dados de altura de pais e filhos coletados
### por Francis Galton, século XIX.


require(HistData)
data("GaltonFamilies")
help("GaltonFamilies")
attach(GaltonFamilies)

x11()
library(car)
scatterplot(childHeight ~ midparentHeight | gender, 
            legend.coords=list(x=64, y=78),
            xlab = 'Altura média dos pais', ylab = 'Altura do(a) filho(a)')
### Gráfico de dispersão da altura dos filhos vs altura média dos pais
### segundo o sexo do filho.

childHeight_2 <- ifelse(gender == 'female', 1.08 * GaltonFamilies$childHeight, 
                        GaltonFamilies$childHeight)
### A multiplicação da altura das mulheres por 1.08 tem por objetivo 
### compensar a menor altura das mulheres (em geral).

scatterplot(childHeight_2 ~ midparentHeight | gender, 
            legend.coords=list(x=64, y=78), xlab = 'Altura média dos pais',
            ylab = 'Altura do(a) filho(a)')
### Gráfico de dispersão da altura dos filhos (corrigida) vs altura média dos pais
### segundo o sexo.

scatterplot(childHeight_2 ~ midparentHeight, 
            legend.coords=list(x=64, y=78), xlab = 'Altura média dos pais',
            ylab = 'Altura do(a) filho(a)')
### Gráfico de dispersão da altura dos filhos (corrigida) vs altura média dos pais
### (desconsiderando o sexo).


ajuste <- lm(childHeight_2 ~ midparentHeight)
### ajuste armazena o resultado da regressão linear para o par de variáveis,
### considerando a altura (corrigida) dos filhos como a variável resposta.

coefficients(ajuste)
### Estimativas de mínimos quadrados para os parâmetros da reta de regressão.

### Podemos expressar o modelo de regressão envolvendo um par de variáveis,
### de maneira alternativa, explicitando a correlação linear entre as variáveis.
### Nesse caso, as estimativas de mínimos quadrados podem ser obtidas 
### usando as seguintes linhas de comando:

r <- cor(childHeight_2, midparentHeight)
sdx <- sd(midparentHeight)
sdy <- sd(childHeight_2)
mx <- mean(midparentHeight)
my <- mean(childHeight_2)

beta1 <- r * sdy/sdx
beta0 <- my - beta1 * mx

plot(childHeight_2 ~ midparentHeight, pch = 20, xlab = 'Altura média dos pais', ylab = 'Altura do(a) filho(a)')
abline(a = beta0, b = beta1, lwd = 2, col = 'red')

### E se a correlação fosse perfeita (igual a 1)?
r <- 1
beta1_r1 <- r * sdy/sdx
beta0_r1 <- my - beta1_r1 * mx
abline(a = beta0_r1, b = beta1_r1, lwd = 2, col = 'red', lty = 2)

### E se não houvesse correlação alguma?

abline(h = mean(childHeight_2), col = 'blue', lwd = 2)
legend(x = 'topleft', legend = c('Ajuste', 'r=1', 'r=0'), lty = c(1,2,1), 
       col = c('red', 'red', 'blue'), lwd = 2)

### Pais com altura acima da média tendem a ter filhos com altura acima da média,
### mas não tão altos quanto eles;
### Pais com altura abaixo da média tendem a ter filhos com altura abaixo da média,
### mas não tão baixos quanto eles.
### Galton definiu esse fenômeno usando a expressão "regressão à média".