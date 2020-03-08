### Nesta aplicação, vamos usar os dados sobre disagnóstico de diabetes
### em uma população de mulheres indígenas norte-americanas. O objetivo
### é predizer o diagnóstico, resultante do teste de glicemia em jejum
### (type) em função das demais covariáveis. Para maiores informações,
### consultar a documentação da base.

require(MASS)
require(glmnet)

pima_dat <- rbind(Pima.tr, Pima.te)
### A principio são duas bases, uma para ajuste e outra para validação.
### Nesta aplicação, no entanto, vamos concatenar as duas bases e proceder
### uma única análise.

head(pima_dat, 15)
summary(pima_dat)

### Vamos usar um glm com resposta binomial e ligação logito (regressão logística) 

### Primeiro, usando todas as covariáveis.
ajuste <- glm(type ~ ., data = pima_dat, family = 'binomial')
logLik(ajuste)
AIC(ajuste)
BIC(ajuste)

### Agora, o modelo nulo, apenas com o intercepto.
ajuste_null <- glm(type ~ 1, data = pima_dat, family = 'binomial')
logLik(ajuste_null)
AIC(ajuste_null)
BIC(ajuste_null)

########################################################################
### Métodos de seleção de covariáveis
### Seleção de covariáveis - método backward
s1a <- step(ajuste, direction = 'backward')
summary(s1a) ### Resumo do modelo produzido pelo método backward.
### Cinco covariáveis foram selecionadas para compor o modelo.

### Vamos repetir o algoritmo backward, mas agora trocando a constante de
### penalização de k = 2 para k = log(n) = 6.27.
s1b <- step(ajuste, direction = 'backward', k = log(nrow(pima_dat)))
summary(s1b)
### Neste caso, quatro covariáveis foram selecionadas para compor o modelo.
### A variável age é selecionada quando k = 2, mas não para k = log(n).


### Seleção de covariáveis - método forward. Para o método forward, o modelo
### de partida é o modelo nulo, e precisamos definir o escopo, referente às
### variáveis consideradas para inclusão.
s2a <- step(ajuste_null, scope = formula(ajuste), direction = 'forward')
summary(s2a)

s2b <- step(ajuste_null, scope = formula(ajuste), direction = 'forward', k = log(nrow(pima_dat)))
summary(s2b)
### Novamente, os modelos diferem quanto à inclusão da variável age.

### Finalmente, o método stepwise combinado.
s3a <- step(ajuste_null, scope = formula(ajuste), direction = 'both')
summary(s3a)

s3b <- step(ajuste_null, scope = formula(ajuste), direction = 'both', k = log(nrow(pima_dat)))
summary(s3b)
### Os resultados coincidem com os produzidos pelos outros algoritmos.

########################################################################
########################################################################
########################################################################
### Métodos de regularização - o conflito entre vício e variância.

### Esta simulação tem por objetivo ilustrar o tradeoff (conflito) entre
### viés e variância, na estimação dos parâmetros do modelo.

### Vamos simular dados de duas covariáveis (X1,X2) com distribuição normal,
### vetor de médias (0,0), variâncias iguais a 1 e covariância (e correlação)
### igual a 0.5.

### Condicional aos valores das covariáveis, vamos simular valores de uma
### resposta binária, segundo o seguinte glm: y|x1,x2 ~  binomial(1, mu_x),
### com logito(mu_x) = 1 + 0.5*x1 - 0.25*x2 (regressão logística).

### Essa simulação vai ser repetida 100 vezes. Em cada simulação, vamos
### extrair as estimativas dos parâmetros do modelo ajustado via regressão
### ridge. Para isso, uma sequência crescente de valores para o parâmetro
### de regularização será considerada.

### Ao término da simulação, com base nas 100 rodadas vamos calcular o viés,
### a variância e o erro quadrático médio das estimativas geradas para 
### beta_1 e para beta_2 sob cada valor de lambda.

set.seed(1) ### Fixando a semente, para fins de reprodução.
x <- mvrnorm(100, mu = c(0,0), Sigma = matrix(c(1,0.5,0.5,1), nrow = 2))
x
### Valores simulados das covariáveis, a partir da distribuição normal
### bivariada.

colnames(x) <- c('x1', 'x2')
eta_x <-1 + 0.5*x[,1] - 0.25*x[,2]
eta_x
### Vetor referente ao preditor linear.

log_lambdas <- seq(-5, -0.5, length.out = 30)
### O grid de valores para lambda foi criado na escala de log(lambda).
### Devido ao comportamento dos resultados para diferentes valores desse
### parâmetro, a escala logarítmica é uma escolha apropriada.

beta1 <- matrix(nrow = 100, ncol = length(log_lambdas))
beta2 <- matrix(nrow = 100, ncol = length(log_lambdas))
### <atrizes que vão armazenar as 100 estimativas de beta_1 e as 100 estimativas
### de beta_2 produzidas para cada valor de lambda.

### O laço abaixo repete por 100 vezes a simulação de valores para a resposta
### (y), o ajuste do glm ridge usando o vetor de respostas simuladas e o
### armazenamento, nas matrizes criadas, das estimativas de beta_1 e beta_2
### para cada valor de lambda no grid criado.
for(j in 1:100){
    y <- rbinom(100, 1, exp(eta_x)/(exp(eta_x)+1))
    mod <- glmnet(x, y, family = 'binomial', alpha = 0)
    betas <- coef(mod, s = exp(log_lambdas))
    beta1[j,] <- betas[2,]
    beta2[j,] <- betas[3,]
}

### As funções abaixo servem para calcular viés, variância e erro quadrático
### médio com base nos dados simulados.

bias_beta <- function(vet_beta, beta)
    (mean(vet_beta) - beta)^2

var_beta <- function(vet_beta)
    var(vet_beta)

eqm_beta <- function(vet_beta, beta)
    mean((vet_beta - beta)^2)


### Agora, vamos aplicar as funções criadas para calcular as quantidades de
### interesse para cada parâmetro do modelo e valor de lambda.

### Para beta_1
bias_b1 <- apply(beta1, 2, bias_beta, beta = 0.5)
var_b1 <- apply(beta1, 2, var_beta)
eqm_beta1 <- apply(beta1, 2, eqm_beta, beta = 0.5)

x11(width = 12, height = 12)    
par(cex = 1.2, las = 1)
plot(log_lambdas, bias_b1, type = 'l', lwd = 2, ylab = '')
lines(log_lambdas, var_b1, type = 'l', col = 'red', lwd = 2)
lines(log_lambdas, eqm_beta1, type = 'l', col = 'blue', lwd = 2)
legend(x = 'topleft', lty = 1, lwd = 2, col = c('black', 'red', 'blue'),
       legend = c('Viés', 'Variância', 'EQM'))

### Para beta_2
bias_b2 <- apply(beta2, 2, bias_beta, beta = -0.25)
var_b2 <- apply(beta2, 2, var_beta)
eqm_beta2 <- apply(beta2, 2, eqm_beta, beta = -0.25)

x11(width = 12, height = 12)    
par(cex = 1.2, las = 1)
plot(log_lambdas, bias_b2, type = 'l', lwd = 2, ylab = '', ylim = c(0,0.085))
lines(log_lambdas, var_b2, type = 'l', col = 'red', lwd = 2)
lines(log_lambdas, eqm_beta2, type = 'l', col = 'blue', lwd = 2)
legend(x = 'topleft', lty = 1, lwd = 2, col = c('black', 'red', 'blue'),
       legend = c('Viés', 'Variância', 'EQM'))
### Observe que o viés é mínimo quando o parâmetro de regularização (lambda)
### é pequeno, enquanto a variância é máxima nesse cenário. À medida que aumentamos
### o valor de lambda, o viés aumenta e a variância vai a zero. Ao analisar o
### comportamento do erro quadrático médio, observamos que valores intermediários
### de lambda remetem a menores valores de EQM, ainda que as estimativas
### geradas apresentem algum grau de viés.

########################################################################
########################################################################
########################################################################
### Métodos de regularização - aplicação.

### Para a aplicação dos métodos de regularização, vamos usar todas as 
### covariáveis, exceto pedigree. 

x <- model.matrix(type ~ npreg + glu + bp + skin + bmi + age, data = pima_dat)[, -1]
### Matriz de covariáveis. Observe a exclusão da primeira coluna, que corresponde à
### coluna de uns (referente ao intercepto), que não deve ser incluída.

y <- ifelse(pima_dat$type == 'Yes', 1, 0)
### Convertendo a variável resposta para um vetor numérico. Será necessário
### para o processo de validação cruzada.

g1 <- glmnet(x, y, family = 'binomial', alpha = 1)
### Lasso regression (penalização de primeira ordem)

x11(width = 12, height = 12)
par(cex = 1.5, las = 1)
plot(g1, las = 1, lwd = 2, label=TRUE)
### As trajetórias indicam as estimativas dos betas para diferentes valores
### do termo de penalização. Os valores que identificam as linhas se referem
### às variáveis, na ordem em que foram declaradas na especificação do modelo.
### As variáveis três e quatro ("bp" e "skin") são as primeiras a se igualarem a
### zero, enquanto a variável "glu" é a última. Vamos ver um gráfico semelhante, 
### mas agora em função de lambda, o parâmetro de penalização.

plot(g1, xvar="lambda", label=TRUE, lwd = 2, cex = 20)

### Vamos ver como ficaria o modelo ajustado para diferentes valores de lambda:
coef(g1, s=exp(-7)) ### Lambda = exp(-7) = 0.0009
coef(g1, s=exp(-4)) ### Lambda = exp(-4) = 0.0183
coef(g1, s=exp(-2)) ### Lambda = exp(-2) = 0.1353

### Usando a função glmnet, vamos determinar o valor de lambda que produz
### menor deviance, estimado via validação cruzada.
cvfit <- cv.glmnet(x, y, family = 'binomial', alpha = 1, nfolds = 20) 
### Ao especificar nfolds = 532, estamos usando a estratégia "leave one out".
plot(cvfit)
cvfit$lambda.min 
### Lambda ótimo.

coef(g1, s=cvfit$lambda.min)
### Modelo ajustado com o lambda ótimo.

########################################################################
### Agora, regressão ridge. Para isso, setamos alpha = 0.
g2 <- glmnet(x, y, family = 'binomial', alpha = 0)

x11(width = 12, height = 12)
par(cex = 1.5, las = 1)
plot(g2, las = 1, lwd = 2, label=TRUE)
plot(g2, xvar="lambda", label=TRUE, lwd = 2, cex = 20)

### Validação cruzada.
cvfit2 <- cv.glmnet(x, y, family = 'binomial', alpha = 0, nfolds = 20) 
plot(cvfit2)
cvfit2$lambda.min 
### Lambda ótimo.

coef(g2, s=cvfit2$lambda.min)
### Modelo ajustado com o lambda ótimo.

########################################################################
### Finalizando, para alpha = 0.5.
g3 <- glmnet(x, y, family = 'binomial', alpha = 0.5)

x11(width = 12, height = 12)
par(cex = 1.5, las = 1)
plot(g3, las = 1, lwd = 2, label=TRUE)
plot(g3, xvar="lambda", lwd = 2, label=TRUE, cex = 20)

### Validação cruzada.
cvfit3 <- cv.glmnet(x, y, family = 'binomial', alpha = 0.5, nfolds = 20) 
plot(cvfit3)
cvfit3$lambda.min 
### Lambda ótimo.

coef(g3, s=cvfit3$lambda.min)
### Modelo ajustado com o lambda ótimo.


########################################################################
########################################################################
########################################################################
### Agora, uma aplicação em dados simulados. Primeiramente, vamos simular
### valores para 20 variáveis explicativas. Vamos considerar distribuição
### normal para cada uma delas, com média zero e variância 1. Adicionalmente,
### vamos fixar covariância igual a 0.5 para cada par de variáveis (como
### as variâncias são iguais a um, então isso equivale a covariâncias 
### iguais a 0.5). A função mvrnorm, do pacote MASS, permite simular amostras
### da distribuição normal multivariada, como é o caso.

set.seed(225)
### Fixando uma semente, para que os resultados sejam reproduzíveis.

n <- 2018
### Tamanho da amostra.

mat_cov <- matrix(0.5, nrow = 20, ncol = 20)
diag(mat_cov) <- 1
mat_cov
### Matriz de variâncias e covariâncias.

medias <- rep(0, 20)
### Vetor de médias.

x <- data.frame(mvrnorm(n, mu = medias, Sigma = mat_cov))
dados_simul <- round(x,3)
names(dados_simul) <- paste('x', 1:20, sep = '')
### Dados simulados para as 20 variáveis explicativas.

### Agora, condicional aos valores simulados para as variáveis explicativas,
### vamos simular resultados de uma variável resposta binária, conforme
### a seguinte especificação:

### y|x ~ binomial(m = 1, pi = mu_x)
### logito(mu_x) = eta_x = x1 + 0.4*x3 - 0.5*x7 + 0.25*x10 + 0.25*x19,
### de forma que mu_x = exp(eta_x)/(exp(eta_x)+1).

eta_x <- with(dados_simul, 0.5*x1 + 0.3*x3 - 0.5*x7 - 0.5*x10 + 0.3*x19)
mu_x <- exp(eta_x)/(exp(eta_x) + 1)
dados_simul$y <- rbinom(n, 1, mu_x)
dados_simul$y
### Valores simulados para a resposta.

### Nosso objetivo aqui é aplicar os métodos de seleção de covariáveis e
### de regularização estudados em sala de aula e avaliar o quanto eles são
### capazes, nesta aplicação, de identificar as variáveis usadas na simulação.

### Ajuste do glm com todas as covariáveis.
ajuste <- glm(y~., family = binomial, data = dados_simul)
summary(ajuste)

### Ajuste do modelo nulo.
ajuste_null <- glm(y~1, family = binomial, data = dados_simul)
summary(ajuste_null)

### Método 1: Backward com k = 2 (AIC)
mod1a <- step(ajuste, direction = 'backward')
summary(mod1a)

### Método 2: Backward com k = log(300) (BIC)
mod1b <- step(ajuste, direction = 'backward', k = log(n))
summary(mod1b)

### Método 3: Stepwise com k = 2 (AIC)
mod2a <- step(ajuste, direction = 'both')
summary(mod2a)

### Método 4: Stepwise com k = log(300) (BIC)
mod2b <- step(ajuste, direction = 'both', k = log(500))
summary(mod2b)

### Método 5: Forward com k = 2 (AIC)
mod3a <- step(ajuste_null, scope = formula(ajuste), direction = 'forward')
summary(mod3a)

### Método 6: Forward com k = log(300) (BIC)
mod3b <- step(ajuste_null, scope = formula(ajuste), direction = 'forward', k = log(n))
summary(mod3b)


########################################################################
### Agora, usando o método lasso.

x <- model.matrix(ajuste)[, -1]
### Matriz de covariáveis. Observe a exclusão da primeira coluna, que corresponde à
### coluna de uns (referente ao intercepto), que não deve ser incluída.

y <- dados_simul$y

mod4a <- glmnet(x, y, family = 'binomial', alpha = 1)
### Lasso regression (penalização de primeira ordem)

x11(width = 12, height = 12)
par(cex = 1.5, las = 1)

plot(mod4a, las = 1, lwd = 2, label=TRUE)
plot(mod4a, xvar="lambda", label=TRUE, lwd = 2, cex = 20)
### Gráficos das estimativas vs termo de penalização e vs lambda.

### Usando validação cruzada para determinar o valor de lambda.
cvfit <- cv.glmnet(x, y, family = 'binomial', alpha = 1, nfolds = 20) 
plot(cvfit)
cvfit$lambda.min 
### Lambda ótimo.

coef(mod4a, s = cvfit$lambda.min)
### Modelo ajustado usando o lambda indicado pela validação cruzada.

coef(mod4a, s = 0.015)
### Solução obtida fixando lambda = 0.015.