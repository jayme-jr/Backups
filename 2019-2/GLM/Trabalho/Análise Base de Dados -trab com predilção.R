library(tidyverse)
library(caret)
require(hnp)

### Dados sobre heart desease
dados <- read.csv2("dadosVF.csv",header = T, sep=",",dec=".")
dim(dados)

dados$num <- factor(dados$num)
summary(dados$num)
levels(dados$num) <- c('Yes', 'No'); summary(dados$num)
dados$num <- relevel(dados$num, ref = 'No')
summary(dados$num)
summary(dados)
# Ajustando as variaveis para factor

dados$sex <- as.factor(dados$sex)
summary(dados$sex)
levels(dados$sex) <- c('Mulher', 'Homem');summary(dados$sex)
dados$sex <- relevel(dados$sex, ref = 'Mulher')
summary(dados$sex)

dados$cp <- factor(dados$cp)
summary(dados$cp)
levels(dados$cp) <- c('1', '2','3','4');summary(dados$cp)
class(dados$cp)

dados$fbs <- factor(dados$fbs)
summary(dados$fbs)
levels(dados$fbs) <- c('0', '1');summary(dados$fbs)
class(dados$fbs)

dados$restecg <- factor(dados$restecg)
summary(dados$restecg)
levels(dados$restecg) <- c('0', '1','2');summary(dados$restecg)
class(dados$restecg)

dados$exang <- factor(dados$exang)
summary(dados$exang)
levels(dados$exang) <- c('0', '1');summary(dados$exang)
class(dados$exang)

dados$slope <- factor(dados$slope)
summary(dados$slope)
levels(dados$slope) <- c('1', '2','3');summary(dados$slope)
class(dados$slope)

dados$thal <- factor(dados$thal)
summary(dados$thal)
levels(dados$thal) <- c('3', '6','9');summary(dados$thal)
class(dados$thal)


### Vamos separar, aleatoriamente, a base em duas: uma para o ajuste do 
### modelo (com 208 observações) e outra para validação (com 89 observações).

set.seed(123)
indices <- sample(1:length(dados$num), 
                  size = round(length(dados$age)*0.75)) 


### indices é um vetor com números de 1 a 313 numa sequência aleatória.

dadosajuste <- dados[indices,]
dim(dadosajuste)
### dataframe com as 208 linhas para ajuste.

dadosvalid <- dados[-indices,]
dim(dadosvalid)
### dataframe com 89 linhas, apenas para validação.

########################################################################

### Vamos ajustar um modelo de regressão logística, com as covariáveis selecionadas
### via Backward(AIC)



ajuste_logito <- step(glm(num ~ .,family=binomial(link = logit),data = dadosajuste), direction = 'backward') 
ajuste_probito <- step(glm(num ~ .,family=binomial(link = probit),data = dadosajuste), direction = 'backward') 
ajuste_cloglog <- step(glm(num ~ .,family=binomial(link = cloglog),data = dadosajuste), direction = 'backward') 
ajuste_cauchit <- step(glm(num ~ .,family=binomial(link = cauchit),data = dadosajuste), direction = 'backward') 
AIC(ajuste_logito,ajuste_probito,ajuste_cloglog,ajuste_cauchit)
ajuste <- step(glm(num ~ .,family=binomial(link = cauchit),data = dadosajuste), direction = 'backward') 
data.frame(ajuste$y, dadosajuste$num)
#install.packages("hnp")
hnp(ajuste)
ajuste$family
summary(ajuste)
summary(dados)


predp <- predict(ajuste, newdata = dadosvalid, type = 'response')
predp
### Tabela de confusão e medidas de performance preditiva.
## Metodo manual
class <- ifelse(predp > 0.5, 'Yes', 'No')
tabcruz <- table(class, dadosvalid$num)
tabcruz
acur <- sum(diag(tabcruz))/sum(tabcruz); acur ### Acurácia.
sens <- tabcruz[2,2]/(tabcruz[1,2] + tabcruz[2,2]); sens ### Sensibilidade.
espc <- tabcruz[1,1]/(tabcruz[1,1] + tabcruz[2,1]); espc ### Especificidade.
## Metodo automatico
tabcruz_auto <- confusionMatrix(as.factor(class),dadosvalid$num);tabcruz_auto
res.roc <- roc(dadosvalid$num, predp)

summary(ajuste)
### Resumo do ajuste.

paj2 <- predict(ajuste, type='response', newdata=dadosajuste)
### Probabilidades estimadas para os indivíduos da base de ajuste;

pval2 <-predict(ajuste, type='response', newdata=dadosvalid) 
### Probabilidades estimadas para os indivíduos da base de validação.

round(pval2 * 1000)

################################################################################


### Vamos usar o pacote ROCR para calcular algumas medidas de qualidade preditiva.
performance1 <- performance(pred, 'auc')
performance1

predp <- predict(ajuste, newdata = dadosvalid, type = 'response')
predp

class2 <- ifelse(predp > 0.148, 'Yes', 'No')
tabcruz2 <- table(class2, dadosvalid$num)
tabcruz2
acur2 <- sum(diag(tabcruz2))/sum(tabcruz2); acur2 ### Acurácia.
sens2 <- tabcruz2[2,2]/(tabcruz2[1,2] + tabcruz2[2,2]); sens2 ### Sensibilidade.
espc2 <- tabcruz2[1,1]/(tabcruz2[1,1] + tabcruz2[2,1]); espc2 ### Especificidade.
c(acur,acur2)
c(sens,sens2)
c(espc,espc2)
# Extraindo a área sob a curva.
tabcruz_auto2 <- confusionMatrix(as.factor(class2),dadosvalid$num); tabcruz_auto2

# Gráfico da curva ROC
graf_roc <- plot.roc(res.roc, print.auc = TRUE, print.thres = 'best', main="Curva ROC")

# Analise apenas para fins didáticos
predaj <- prediction(paj2, dadosajuste$num)
perfaj <- performance(predaj, measure = "tpr" , x.measure = "fpr") 
performance(predaj, 'auc')

plotar <- function(func){
  x11()
  par(mfrow=c(2,2))
  plot(func,1)
  plot(func,2)
  plot(func,3)
  plot(func,4)
  par(mfrow=c(1,1))
}

# outputs para montar relatorio e apresentaçao 

hnp(ajuste, main="Envelopes Simulados")
summary(ajuste)
AIC(ajuste_logito,ajuste_probito,ajuste_cloglog,ajuste_cauchit)
graf_roc <- plot.roc(res.roc, print.auc = TRUE, print.thres = 'best', main="Curva ROC",ylab="Sensibilidade",xlab="Especificidade")
tabcruz_auto2
tabcruz_auto
summary(dados)
plot(dados$oldpeak)
