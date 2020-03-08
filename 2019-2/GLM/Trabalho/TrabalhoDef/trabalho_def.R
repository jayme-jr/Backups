library(tidyverse)
library(drc)
library(utils)
library(kableExtra)
library(combinat)
library(tinytex)
library(MASS)
library(gridExtra)
library(emmeans)
library(car)
library(ROCR)
library(pROC)
library(caret)
library(hnp)
library(gamlss)
##### Bank Marketing ######
### Base para predizer se o cliente vai assinar ou não com o 
### banco baseado nas informações do cliente.
#### Variáveis Explicativa ####
## age: idade do cliente - numérico inteiro;
## job: emprego do cliente - admin., blue-collar, entrepreneur, housemaid, 
##                           management, retired, self-employed, services, 
##                           student, technician, unemployed, unknown;
## marital: estado civil do cliente - divorced, married, single, unknown;
## education: nível educacional do cliente - primary, secondary, tertiary, unknown;
## default: cliente possui crédito - no, yes, unknown;
## balance: balanço anual médio do cliente em euro - numérico;
## housing: cliente possui empréstimo habitacional - no, yes, unknown;
## loan: cliente possui empréstimo pessoal - no, yes, unknown;
## contact: forma de contato com o cliente - unknown, telephone, cellular;
## day: dia do mês - inteiro entre 1 e 31;
## month: mês do ano - jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec;
## duration: duração do último contato com o cliente - numérico em segundos;
## campaing: número de contatos feitos com o cliente - numérico inteiro;
## pdays: número de dias desde o último contato feito com o cliente em uma campanha
##        passada - numérico inteiro(-1 significa que não houve contato prévio);
### Variável Resposta ###
## y: classificação do preditor - yes, no.

data <- read.csv2("bank_marketing_dataset.csv", sep = ",", dec = ".") %>% 
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug",
                                          "sep", "oct", "nov", "dec")))
no <- which(names(data) == "previous" | names(data) == "poutcome")
b_mark <- data[,-no]

### Vamos separar, aleatoriamente, a base em duas: uma para o ajuste do 
### modelo (com 80% das observações) e outra para validação (com 20% observações).
mod_obs <- ceiling(dim(b_mark)[1]*0.8)

set.seed(1234)
indices <- sample(1:dim(b_mark)[1], size = mod_obs) 
### indices é um vetor com números de 1 a 4521 numa sequência aleatória.

bmarkajuste <- b_mark[indices,]
### dataframe com as 3617 linhas para ajuste.

bmarkvalid <- b_mark[-indices,]
### dataframe com 904 linhas, apenas para validação.

##### Ajustes Iniciais ######

ajuste_null <- glm(y ~ 1, data = bmarkajuste, family = 'binomial')
ajuste_null_prob <- glm(y ~ 1, data = bmarkajuste, family = 'binomial'(link = probit))
ajuste_null_clog <- glm(y ~ 1, data = bmarkajuste, family = 'binomial'(link = cloglog))
ajuste_null_cauc <- glm(y ~ 1, data = bmarkajuste, family = 'binomial'(link = cauchit))

ajuste1 <- glm(y ~ .,family=binomial,data = bmarkajuste)
ajuste1_prob <- glm(y ~ .,family=binomial(link = probit),data = bmarkajuste)
ajuste1_clog <- glm(y ~ .,family=binomial(link = cloglog),data = bmarkajuste)
ajuste1_cauc <- glm(y ~ .,family=binomial(link = cauchit),data = bmarkajuste)

ajuste4 <- step(ajuste_null, scope = formula(ajuste1), direction = "both")
ajuste_probito <- step(ajuste_null_prob, scope = formula(ajuste1_prob), direction = 'both') 
ajuste_cloglog <- step(ajuste_null_clog, scope = formula(ajuste1_clog), direction = 'both')
ajuste_cauchit <- step(ajuste_null_cauc, scope = formula(ajuste1_cauc), direction = 'both')

####### Tablelas Ajustes Iniciais ############

AIC_ <- data.frame(AIC(ajuste4,ajuste_probito,ajuste_cloglog,ajuste_cauchit)) %>% 
  mutate(Lig = c("Logito", "Probito", "Clog-log", "Cauchit"),
         `N° de Parâmetros` = c(length(ajuste4$coefficients), length(ajuste_probito$coefficients),
                                length(ajuste_cloglog$coefficients), length(ajuste_cauchit$coefficients))) %>% 
  arrange(AIC) %>% 
  dplyr::select(`Função de Ligação` = Lig, AIC, `N° de Parâmetros`)


T_1 <- kableExtra::kable(AIC_, align = 'ccc', booktabs = TRUE) %>% 
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))

###### Agrupamento de Níveis Usando GAMLLS #####
# Variável month
m11 <- gamlss(y~pcat(month, method="GAIC", k=2), family = BI(mu.link = "probit"), data=bmarkajuste) # GAIC
mn <- gamlss(y~1, family = BI(mu.link = "probit"), data=bmarkajuste) # null model 
ms <- gamlss(y~month-1, family = BI(mu.link = "probit"), data=bmarkajuste) # saturated model 
m1 <- gamlss(y~pcat(month), family = BI(mu.link = "probit"), data=bmarkajuste)
AIC(mn, ms, m1, m11)

groups_m <- unique(getSmo(m11)$coef)
coefs_m <- round(getSmo(m11)$coef, digits = 2)
month_lambda <- round(getSmo(m11)$lambda, digits = 3)
month_tbl <- data.frame(Niveis = levels(bmarkajuste$month), Coeficientes = coefs_m) %>% arrange(Coeficientes)

# variável job
m11j <- gamlss(y~pcat(job, method="GAIC", k=2), family = BI(mu.link = "probit"), data=bmarkajuste)
mnj <- gamlss(y~1, family = BI(mu.link = "probit"), data=bmarkajuste) # null model 
msj <- gamlss(y~job-1, family = BI(mu.link = "probit"), data=bmarkajuste) # saturated model 
m1j <- gamlss(y~pcat(job), family = BI(mu.link = "probit"), data=bmarkajuste)
AIC(mnj, msj, m1j, m11j)

coefs_j <- round(getSmo(m11j)$coef, digits = 2)
job_lambda <- round(getSmo(m11j)$lambda, digits = 3)
job_tbl <- data.frame(Niveis = levels(bmarkajuste$job), Coeficientes = coefs_j) %>% arrange(Coeficientes)

## Tabela Agrupamento month

T_2 <- kableExtra::kable(month_tbl, align = 'cc', booktabs = TRUE) %>% 
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))

## Tabela Agrupamento job

T_3 <- kableExtra::kable(job_tbl, align = 'cc', booktabs = TRUE) %>% 
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))

### Reorganizando as variáveis month e job ####
m_g1 <- "may"
m_g2 <- c("jan", "jun", "jul", "nov") 
m_g3 <- "aug"
m_g4 <- c("feb", "apr")
m_g5 <- c("mar", "sep", "dec")

j_g1 <- "blue-collar"
j_g2 <- c("entrepreneur", "sevices", "tehnician")
j_g3 <- c("admin", "housemaid", "management", "self-employed", "unemployed", "unknow")
j_g4 <- "retired"
j_g5 <- "student"

bmarkajuste2 <- bmarkajuste %>% 
  mutate(month = ifelse(month %in% m_g2, "month2",
                        ifelse(month %in% m_g4, "month4",
                               ifelse(month %in% m_g5, "month5",
                                      ifelse(month == "may", "month1", "month3")))),
         month = factor(month, levels = c("month1", "month2", "month3", "month4", "month5")),
         job = ifelse(job %in% j_g2, "job2",
                      ifelse(job %in% j_g3, "job3",
                             ifelse(job == "blue-collar", "job1",
                                    ifelse(job == "retired", "job4", "job5")))),
         job = factor(job, levels = c("job1", "job2", "job3", "job4", "job5")))

bmarkvalid2 <- bmarkvalid %>% 
  mutate(month = ifelse(month %in% m_g2, "month2",
                        ifelse(month %in% m_g4, "month4",
                               ifelse(month %in% m_g5, "month5",
                                      ifelse(month == "may", "month1", "month3")))),
         month = factor(month, levels = c("month1", "month2", "month3", "month4", "month5")),
         job = ifelse(job %in% j_g2, "job2",
                      ifelse(job %in% j_g3, "job3",
                             ifelse(job == "blue-collar", "job1",
                                    ifelse(job == "retired", "job4", "job5")))),
         job = factor(job, levels = c("job1", "job2", "job3", "job4", "job5")))

##### Ajustes Novos ######

ajuste_null_n <- glm(y ~ 1, data = bmarkajuste2, family = 'binomial')
ajuste_null_prob_n <- glm(y ~ 1, data = bmarkajuste2, family = 'binomial'(link = probit))
ajuste_null_clog_n <- glm(y ~ 1, data = bmarkajuste2, family = 'binomial'(link = cloglog))
ajuste_null_cauc_n <- glm(y ~ 1, data = bmarkajuste2, family = 'binomial'(link = cauchit))

ajuste1_n <- glm(y ~ .,family=binomial,data = bmarkajuste2)
ajuste1_prob_n <- glm(y ~ .,family=binomial(link = probit),data = bmarkajuste2)
ajuste1_clog_n <- glm(y ~ .,family=binomial(link = cloglog),data = bmarkajuste2)
ajuste1_cauc_n <- glm(y ~ .,family=binomial(link = cauchit),data = bmarkajuste2)

ajuste_logito_n <- step(ajuste_null_n, scope = formula(ajuste1_n), direction = "both")
ajuste_probito_n <- step(ajuste_null_prob_n, scope = formula(ajuste1_prob_n), direction = 'both') 
ajuste_cloglog_n <- step(ajuste_null_clog_n, scope = formula(ajuste1_clog_n), direction = 'both')
ajuste_cauchit_n <- step(ajuste_null_cauc_n, scope = formula(ajuste1_cauc_n), direction = 'both')

####### Tablelas Ajustes Iniciais ############

AIC_ <- data.frame(AIC(ajuste_probito, ajuste_logito_n, ajuste_probito_n, ajuste_cloglog_n, ajuste_cauchit_n)) %>% 
  mutate(Lig = c("Probito *", "Logito", "Probito", "Complemento log-log", "Cauchit"),
         `N° de Parâmetros` = c(length(ajuste_probito$coefficients),length(ajuste_logito_n$coefficients),
                                length(ajuste_probito_n$coefficients), length(ajuste_cloglog_n$coefficients), 
                                length(ajuste_cauchit_n$coefficients))) %>% 
  arrange(AIC) %>%
  dplyr::select(`Função de Ligação` = Lig, AIC, `N° de Parâmetros`)


T_4 <- kableExtra::kable(AIC_, align = 'ccc', booktabs = TRUE) %>% 
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))

### Parâmetros do modelo e seus coeficientes

coefic <- data.frame(summary(ajuste_probito_n)$coefficients)

linha <- rownames(coefic)

coefic <- coefic %>% 
  rename(Estimativa = Estimate, `Erro Padrão` = Std..Error) %>% 
  mutate_at(c("Estimativa", "Erro Padrão"), function(x) round(x, digits = 3)) %>% 
  dplyr::select(-c(z.value, Pr...z..))
  
rownames(coefic) <- linha

T_5 <- kableExtra::kable(coefic, align = 'cc', booktabs = TRUE) %>% 
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))

##### Gráficos de efeito #####

plot(predictorEffects(ajuste_probito_n, ~ duration + month + job + pdays))
plot(predictorEffects(ajuste_probito_n, ~ housing + campaign + loan + age))

##### Sensibilidade e Especificidade ######

paj <- predict(ajuste_probito_n, type = 'response', newdata = bmarkajuste2)
### Probabilidades estimadas para os indivíduos da base de ajuste;

pval <-predict(ajuste_probito_n, type = 'response', newdata = bmarkvalid2) 
### Probabilidades estimadas para os indivíduos da base de validação.

### Vamos cruzar realidade e predição para diferentes pontos de corte.

classp0.5 <- factor(ifelse(pval >= 0.5, 'no', 'yes'))
# classp0.5 ### Classificações usando o ponto de corte 0,5.
# data.frame(pval, classp0.5) ### Probabilidades estimadas e classificações.

### Classificando como "no" indivíduos com p > 0,5.
tabela.5 <- table(classp0.5, bmarkvalid2$y) 

T_6 <- kableExtra::kable(tabela.5, align = 'cc', booktabs = TRUE) %>% 
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))

### Vamos estimar a sensibilidade e a especificidade referentes a esta regra de decisão.
sensp0.5 <- sum(classp0.5 == 'no' & bmarkvalid2$y == 'no')/sum(bmarkvalid2$y == 'no')
sensp0.5
espec0.5 <- sum(classp0.5 == 'yes' & bmarkvalid2$y == 'yes')/sum(bmarkvalid2$y == 'yes')
espec0.5

## Baseado nos dados de ajuste, chegamos a conclusão de usar uma probabilidade > 0.89

classp0.89 <- factor(ifelse(pval >= 0.89, 'no', 'yes'))
# classp0.89 ### Classificações usando o ponto de corte 0,89.
# data.frame(pval, classp0.89) ### Probabilidades estimadas e classificações.

### Classificando como "No" indivíduos com p > 0,89.
 tabela.89 <- table(classp0.89, bmarkvalid2$y) 

 T_7 <- kableExtra::kable(tabela.89, align = 'cc', booktabs = TRUE) %>% 
   kableExtra::kable_styling(latex_options = c("striped", "hold_position"))

### Vamos estimar a sensibilidade e a especificidade referentes a esta regra de decisão.
sensp0.89 <- sum(classp0.89 == 'no' & bmarkvalid2$y == 'no')/sum(bmarkvalid2$y == 'no')
sensp0.89
espec0.89 <- sum(classp0.89 == 'yes' & bmarkvalid2$y == 'yes')/sum(bmarkvalid2$y == 'yes')
espec0.89

## Um terceiro ponto de corte com probabilidade > 0.3

classp0.3 <- factor(ifelse(pval >= 0.3, 'no', 'yes'))
# classp0.89 ### Classificações usando o ponto de corte 0,89.
# data.frame(pval, classp0.3) ### Probbilidades estimadas e classificações.

### Classificando como "No" indivíduos com p > 0,89.
tabela.3 <- table(classp0.3, bmarkvalid2$y)

T_8 <- kableExtra::kable(tabela.3, align = 'cc', booktabs = TRUE) %>% 
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))

### Vamos estimar a sensibilidade e a especificidade referentes a esta regra de decisão.
sensp0.3 <- sum(classp0.3 == 'no' & bmarkvalid2$y == 'no')/sum(bmarkvalid2$y == 'no')
sensp0.3
espec0.3 <- sum(classp0.3 == 'yes' & bmarkvalid2$y == 'yes')/sum(bmarkvalid2$y == 'yes')
espec0.3

## Quadro para comparação das probabilidades para sensibilidade e especificidade

datacomp <- data.frame(c(sensp0.89,sensp0.5, sensp0.3),
                       c(espec0.89,espec0.5, espec0.3))
names(datacomp)<-c('Sensibilidade','Especificidade')
rownames(datacomp)<-c('pc=0,89','pc=0,5', 'pc=0,3')
datacomp

T_9 <- kableExtra::kable(datacomp, align = 'ccc', booktabs = TRUE) %>% 
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))

### Vamos usar o pacote ROCR para calcular algumas medidas de qualidade preditiva.### 

pred <- prediction(pval, bmarkvalid2$y)
perf <- performance(pred, measure = "tpr" , x.measure = "fpr") 

# usando a base de ajuste
predaj <- prediction(paj, bmarkajuste2$y)
perfaj <- performance(predaj, measure = "tpr" , x.measure = "fpr")
# tpr: True Positive Rate; fpr: False Positive Rate.

plot(perf, colorize=TRUE,  print.cutoffs.at=seq(0.05,0.95,0.05), lwd = 2, main="Curva ROC",ylab="Sensibilidade",xlab="Especificidade")
abline(0,1, lty = 2)
plot(perfaj, col = 'red', lwd = 2, add = T)

curva <- performance(pred, 'auc')

# Vamos supor que o custo de classificar um falço não seja 1.5 vezes o de classificar um falso sim 
# (C(S|N) = 1.5 * C(N|S)). Assim, para uma regra de classificação com probabilidades de classificação incorretas P(S|N) e P(N|S), o custo esperado de má classificação (ECMC) fica dado por:
#   
#   ECMC = C(S|N) \* P(S|N) \* P(N) + C(N|S) \* P(N|S) \* P(S)
# 
# Como existe 11% de respostas sim na base, P(S) = 0,11; P(N) = 0,89.
# 
# Assim, ECMC = 0.89 \* P(S|N) + 0,165 \* P(N|S)

perf3 <- performance(pred, measure = 'fpr', x.measure = 'fnr')
PMB <- perf3@y.values[[1]] ### P(M|B) para os diferentes pontos de corte (pc)
PBM <- perf3@x.values[[1]] ### P(B|M) para os diferentes pontos de corte.
pc <- perf3@alpha.values[[1]] ### pontos de corte.

ECMC <- 0.89*PMB + 0.165*PBM ### Custos esperados de má-classificação.

plot(pc, ECMC, main="Custo de Classificação",ylab="ECMC",xlab="Pontos de Corte")

## Ponto de corte em torno de 0.4 gera o menor custo.