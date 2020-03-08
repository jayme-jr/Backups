########################################################################
### Regressão para dados binários - modelo preditivo.
require(kernlab)
require(statmod)
require(pROC)
require(car)
require(glmnet)

### Vamos usar regressão logística para classificar e-mails como spams ou
### não spams segundo as frequências com que diferentes termos e caracteres
### aparecem no corpo da mensagem.

data("spam")
help("spam")
summary(spam)

### Vamos selecionar apenas as covariáveis com frequência média superior
### a 0.2 para a sequência da análise.
i_freq <- which(apply(spam[,1:57],2,mean) > 0.2)
spam2 <- spam[,c(i_freq, 58)]

### Alguns gráficos
with(spam2, boxplot(log(charExclamation + 0.1) ~ type, ylab = 'log(frequência)',
                   col = 'lightblue', cex = 1.45, las = 1))
with(spam2, boxplot(log(capitalTotal + 0.1) ~ type, ylab = 'log(frequência)',
                   col = 'lightblue', cex = 1.45, las = 1))
### Aparentemente, as frequências de pontos de exclamação ou letras maiúsculas
### são importantes para discriminar spams e não spams (spams têm maior
### frequência de ambos).


########################################################################
### Preparação dos dados.

### Vamos dividir a base em duas: uma para ajuste do modelo e outra para
### validação. A base de validação com 1000 observações e a de ajuste com
### as demais.

set.seed(88) ### Fixando a semente para sorteio das observações.
select <- sample(1:nrow(spam2), 1000)
### Linhas da base que serão usadas para validação.

spam_ajuste <- spam2[-select,] ### Base de ajuste.
spam_predict <- spam2[select,] ### Base de validação.

########################################################################
### Ajuste do modelo. Vamos considerar diferentes abordagens:

### Modelo 1 - GLM com todas as covariáveis;
modelo1 <- glm(type ~ ., family = 'binomial', data = spam_ajuste)
pred1 <- predict(modelo1, newdata = spam_predict, type = 'response')
round(pred1,5) ### Predições.

### Tabela de confusão e medidas de performance preditiva.
class1 <- ifelse(pred1 > 0.5, 'Class_spam', 'Class_nspam')
tabcruz <- table(class1, spam_predict$type)
tabcruz
ac1 <- sum(diag(tabcruz))/1000; ac1 ### Acurácia.
s1 <- tabcruz[2,2]/(tabcruz[1,2] + tabcruz[2,2]); s1 ### Sensibilidade.
e1 <- tabcruz[1,1]/(tabcruz[1,1] + tabcruz[2,1]); e1 ### Especificidade.


########################################################################
### Modelo 2 - com seleção do tipo Backward, via AIC.
modelo2 <- step(modelo1, k = 2)
pred2 <- predict(modelo2, newdata = spam_predict, type = 'response')

### Tabela de confusão e medidas de performance preditiva.
class2 <- ifelse(pred2 > 0.5, 'Class_spam', 'Class_nspam')
tabcruz <- table(class2, spam_predict$type)
tabcruz
ac2 <- sum(diag(tabcruz))/1000; ac2 ### Acurácia.
s2 <- tabcruz[2,2]/(tabcruz[1,2] + tabcruz[2,2]); s2 ### Sensibilidade.
e2 <- tabcruz[1,1]/(tabcruz[1,1] + tabcruz[2,1]); e2 ### Especificidade.


########################################################################
### Modelo 3 - com seleção do tipo Backward, via BIC.
modelo3 <- step(modelo1, k = log(nrow(spam_ajuste)))
pred3 <- predict(modelo3, newdata = spam_predict, type = 'response')

### Tabela de confusão e medidas de performance preditiva.
class3 <- ifelse(pred3 > 0.5, 'Class_spam', 'Class_nspam')
tabcruz <- table(class3, spam_predict$type)
tabcruz
ac3 <- sum(diag(tabcruz))/1000; ac3 ### Acurácia.
s3 <- tabcruz[2,2]/(tabcruz[1,2] + tabcruz[2,2]); s3 ### Sensibilidade.
e3 <- tabcruz[1,1]/(tabcruz[1,1] + tabcruz[2,1]); e3 ### Especificidade.

########################################################################
### Modelo 4 - com regularização, método lasso.
x <- model.matrix(type ~ ., data = spam_ajuste)[, -1]
y <- ifelse(spam_ajuste$type == 'spam', 1, 0)

modelo4 <- glmnet(x, y, family = 'binomial', alpha = 1)
cvfit <- cv.glmnet(x, y, family = 'binomial', alpha = 1, nfolds = 10)

newx <- model.matrix(type ~ ., data = spam_predict)[, -1]
pred4 <- predict(modelo4, newx = newx, s = cvfit$lambda.min, type = 'response')

### Tabela de confusão e medidas de performance preditiva.
class4 <- ifelse(pred4 > 0.5, 'Class_spam', 'Class_nspam')
tabcruz <- table(class4, spam_predict$type)
tabcruz
ac4 <- sum(diag(tabcruz))/1000; ac4 ### Acurácia.
s4 <- tabcruz[2,2]/(tabcruz[1,2] + tabcruz[2,2]); s4 ### Sensibilidade.
e4 <- tabcruz[1,1]/(tabcruz[1,1] + tabcruz[2,1]); e4 ### Especificidade.

########################################################################
### Modelo 5 - com regularização, método ridge.
x <- model.matrix(type ~ ., data = spam_ajuste)[, -1]
y <- ifelse(spam_ajuste$type == 'spam', 1, 0)

modelo5 <- glmnet(x, y, family = 'binomial', alpha = 0)
cvfit <- cv.glmnet(x, y, family = 'binomial', alpha = 0, nfolds = 10)

newx <- model.matrix(type ~ ., data = spam_predict)[, -1]
pred5 <- predict(modelo5, newx = newx, s = cvfit$lambda.min, type = 'response')

### Tabela de confusão e medidas de performance preditiva.
class5 <- ifelse(pred5 > 0.5, 'Class_spam', 'Class_nspam')
tabcruz <- table(class5, spam_predict$type)
tabcruz
ac5 <- sum(diag(tabcruz))/1000; ac5 ### Acurácia.
s5 <- tabcruz[2,2]/(tabcruz[1,2] + tabcruz[2,2]); s5 ### Sensibilidade.
e5 <- tabcruz[1,1]/(tabcruz[1,1] + tabcruz[2,1]); e5 ### Especificidade.

########################################################################
### Apanhado geral dos resultados.
data.frame(Modelo = c('Modelo 1', 'Modelo 2', 'Modelo 3', 'Modelo 4', 'modelo 5'),
           Acurácia  = c(ac1, ac2, ac3, ac4, ac5), Sensibilidade = c(s1, s2, s3, s4, s5),
           Especificidade = c(e1, e2, e3, e4, e5))

########################################################################
### Análise via curva ROC - procurando as regras de classificação (pontos 
### de corte) ótimos.

predroc1 <- roc(spam_predict$type, pred1, percent = TRUE)
predroc1$auc
plot(predroc1, print.thres = seq(0.1,0.95,0.05), print.thres.pattern.cex = 0.8)
c1 <- coords(predroc1, 'best'); c1 

predroc2 <- roc(spam_predict$type, pred2, percent = TRUE)
predroc2$auc
c2 <- coords(predroc2, 'best'); c2 

predroc3 <- roc(spam_predict$type, pred3, percent = TRUE)
predroc3$auc
c3 <- coords(predroc3, 'best'); c3 

predroc4 <- roc(spam_predict$type, as.numeric(pred4), percent = TRUE)
predroc4$auc
c4 <- coords(predroc4, 'best'); c4 

predroc5 <- roc(spam_predict$type, as.numeric(pred5), percent = TRUE)
predroc5$auc
c5 <- coords(predroc5, 'best'); c5 

data.frame(Modelo = c('Modelo 1', 'Modelo 2', 'Modelo 3', 'Modelo 4', 'modelo 5'),
           rbind(c1, c2, c3, c4, c5))


########################################################################
### Incorporando custos de má-classificação.

### Custos de má-classificação podem ser incorporados na seleção da melhor 
### regra de classificação.
### A função coords, por default, identifica o ponto de corte tal que a soma
### sensibilidade + especificidade seja máxima. Podemos modificar este critério
### através da função best werights, em que declaramos:

### 1 - O custo relativo de um falso negativo quando comparado a um falso
### positivo;

### 2 - A prevalência ou proporção de casos (spams) na população.

### Vamos considerar alguns cenários. Em todos os casos, Cn|s é o custo de
### classificar um spam como não spam, Cs|n o de classificar como spam um 
### não spam e Ps é a prevalência de spams.

### Cenário 1 - Cn|s = 0.5*Cs|n, Ps = 1/3.
coords(predroc1, x = 'best', best.weights = c(0.5, 1/3)) 

### Cenário 2 - Cn|s = Cs|n, Ps = 1/3.
coords(predroc1, x = 'best', best.weights = c(1, 1/3))

### Cenário 3 - Cn|s = Cs|n, Ps = 1/2.
coords(predroc1, x = 'best', best.weights = c(1, 1/2))

### Cenário 4 - Cn|s = Cs|n, Ps = 1/20.
coords(predroc1, x = 'best', best.weights = c(1, 1/20))

### Cenário 5 - Cn|s = 2*Cs|n, Ps = 1/3.
coords(predroc1, x = 'best', best.weights = c(2, 1/3))

### Cenário 6 - Cn|s = 5*Cs|n, Ps = 1/3.
coords(predroc1, x = 'best', best.weights = c(5, 1/3))

### Cenário 7 - Cn|s = 10*Cs|n, Ps = 1/3.
coords(predroc1, x = 'best', best.weights = c(10, 1/3)) 
