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
library(effects)
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
aux <- data[,no]

head(b_mark)
summary(b_mark)
str(b_mark)

# data.frame(ajuste_probito$y, bmarkajuste$y) verificar qual resposta vale L0 e L1
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

###########################
## Ajustes com base sem manipulação
## Ajuste 1
ajuste1 <- glm(y ~ .,family=binomial,data = bmarkajuste)
# summary(ajuste1)
# 
# ## Ajuste 2(metodo backward)
# ajuste2 <- step(glm(y ~ .,family=binomial,data = bmarkajuste), direction = 'backward')
# summary(ajuste2)

## Ajuste 3(metodo forward)
ajuste_null <- glm(y ~ 1, data = bmarkajuste, family = 'binomial')
# ajuste3 <- step(ajuste_null, scope = formula(ajuste1), direction = "forward")
# summary(ajuste3)

## Ajuste 4(metodo stepwise)
ajuste4 <- step(ajuste_null, scope = formula(ajuste1), direction = "both")
summary(ajuste4)

# AIC(ajuste1, ajuste2, ajuste3, ajuste4)
## No melhor ajuste as variáveis day, age, default, balance e 
## job foram retiradas do modelo

## Ajustes com base ajustada nas variáveis month e job
# q_m1 <- c("jan", "feb", "mar", "apr")
# q_m2 <- c("may", "jun", "jul", "aug")
# q_m3 <- c("sep", "oct", "nov", "dec")
# 
# active <- c("services", "management", "blue-collar", "technician", "entrepreneur", "admin.")
# inactive <- c("unemployed", "retired", "student")
# other <- c("unknown", "housemaid")
# 
# 
# b_mark2 <- data %>% 
#   mutate(m_quarter = ifelse(month %in% q_m1, "Q_1", 
#                             ifelse(month %in% q_m2, "Q_2", "Q_3")),
#          job = ifelse(job %in% active, "Worker", 
#                       ifelse(job %in% inactive, "Inactive",
#                              ifelse(job %in% other, "Other", "self-employed")))) %>% 
#   dplyr::select(-month)
# 
# no2 <- which(names(b_mark2) == "previous" | names(b_mark2) == "poutcome")
# 
# b_mark2 <- b_mark2[,-no2]
# 
# ### Vamos separar, aleatoriamente, a base em duas: uma para o ajuste do 
# ### modelo (com 80% das observações) e outra para validação (com 20% observações).
# # mod_obs2 <- ceiling(dim(b_mark2)[1]*0.8)
# 
# # set.seed(1234)
# # indices2 <- sample(1:dim(b_mark2)[1], size = mod_obs2) 
# 
# ### indices é um vetor com números de 1 a 4521 numa sequência aleatória.
# 
# bmarkajuste2 <- b_mark2[indices,]
# ### dataframe com as 3617 linhas para ajuste.
# 
# bmarkvalid2 <- b_mark2[-indices,]
# ### dataframe com 904 linhas, apenas para validação.
# 
# ###########################
# 
# ## Ajuste 1
# ajuste_m1 <- glm(y ~ .,family=binomial,data = bmarkajuste2)
# summary(ajuste_m1)
# 
# ## Ajuste 2(metodo backward)
# ajuste_m2 <- step(glm(y ~ .,family=binomial,data = bmarkajuste2), direction = 'backward')
# summary(ajuste_m2)
# 
# ## Ajuste 3(metodo forward)
# ajuste_null2 <- glm(y ~ 1, data = bmarkajuste2, family = 'binomial')
# ajuste_m3 <- step(ajuste_null2, scope = formula(ajuste_m1), direction = "forward")
# summary(ajuste_m3)
# 
# ## Ajuste 4(metodo stepwise)
# ajuste_m4 <- step(ajuste_null2, scope = formula(ajuste_m1), direction = "both")
# summary(ajuste_m4)
# 
# AIC(ajuste1, ajuste2, ajuste3, ajuste4, ajuste_m1, ajuste_m2, ajuste_m3, ajuste_m4)
# 
# ######################
# ## Ajuste manipulando somente a variável month
# 
# b_mark3 <- data %>% 
#   mutate(m_quarter = ifelse(month %in% q_m1, "Q_1", 
#                             ifelse(month %in% q_m2, "Q_2", "Q_3"))) %>% 
#   dplyr::select(-month)
# 
# no3 <- which(names(b_mark3) == "previous" | names(b_mark3) == "poutcome")
# 
# b_mark3 <- b_mark3[,-no3]
# 
# ### Vamos separar, aleatoriamente, a base em duas: uma para o ajuste do 
# ### modelo (com 80% das observações) e outra para validação (com 20% observações).
# # mod_obs3 <- ceiling(dim(b_mark3)[1]*0.8)
# 
# # set.seed(1234)
# # indices3 <- sample(1:dim(b_mark3)[1], size = mod_obs3) 
# 
# ### indices é um vetor com números de 1 a 4521 numa sequência aleatória.
# 
# bmarkajuste3 <- b_mark3[indices,]
# ### dataframe com as 3617 linhas para ajuste.
# 
# bmarkvalid3 <- b_mark3[-indices,]
# ### dataframe com 904 linhas, apenas para validação.
# 
# ###########################
# 
# ## Ajuste 1
# ajuste_mon1 <- glm(y ~ .,family=binomial,data = bmarkajuste3)
# summary(ajuste_mon1)
# 
# ## Ajuste 2(metodo backward)
# ajuste_mon2 <- step(glm(y ~ .,family=binomial,data = bmarkajuste3), direction = 'backward')
# summary(ajuste_mon2)
# 
# ## Ajuste 3(metodo forward)
# ajuste_null_mon <- glm(y ~ 1, data = bmarkajuste3, family = 'binomial')
# ajuste_mon3 <- step(ajuste_null_mon, scope = formula(ajuste_mon1), direction = "forward")
# summary(ajuste_mon3)
# 
# ## Ajuste 4(metodo stepwise)
# ajuste_mon4 <- step(ajuste_null_mon, scope = formula(ajuste_mon1), direction = "both")
# summary(ajuste_mon4)
# 
# ## Ajuste manipulando somente a variável job
# 
# b_mark4 <- data %>% 
#   mutate(job = ifelse(job %in% active, "Worker", 
#                       ifelse(job %in% inactive, "Inactive",
#                              ifelse(job %in% other, "Other", "self-employed")))) 
# 
# no4 <- which(names(b_mark4) == "previous" | names(b_mark4) == "poutcome")
# 
# b_mark4 <- b_mark4[,-no4]
# 
# ### Vamos separar, aleatoriamente, a base em duas: uma para o ajuste do 
# ### modelo (com 80% das observações) e outra para validação (com 20% observações).
# # mod_obs3 <- ceiling(dim(b_mark3)[1]*0.8)
# 
# # set.seed(1234)
# # indices3 <- sample(1:dim(b_mark3)[1], size = mod_obs3) 
# 
# ### indices é um vetor com números de 1 a 4521 numa sequência aleatória.
# 
# bmarkajuste4 <- b_mark4[indices,]
# ### dataframe com as 3617 linhas para ajuste.
# 
# bmarkvalid4 <- b_mark4[-indices,]
# ### dataframe com 904 linhas, apenas para validação.
# 
# ###########################
# 
# ## Ajuste 1
# ajuste_job1 <- glm(y ~ .,family=binomial,data = bmarkajuste4)
# summary(ajuste_job1)
# 
# ## Ajuste 2(metodo backward)
# ajuste_job2 <- step(glm(y ~ .,family=binomial,data = bmarkajuste4), direction = 'backward')
# summary(ajuste_job2)
# 
# ## Ajuste 3(metodo forward)
# ajuste_null_job <- glm(y ~ 1, data = bmarkajuste4, family = 'binomial')
# ajuste_job3 <- step(ajuste_null_job, scope = formula(ajuste_job1), direction = "forward")
# summary(ajuste_job3)
# 
# ## Ajuste 4(metodo stepwise)
# ajuste_job4 <- step(ajuste_null_job, scope = formula(ajuste_job1), direction = "both")
# summary(ajuste_job4)
# 
# 
# AIC(ajuste1, ajuste2, ajuste3, ajuste4, ajuste_m1, ajuste_m2, ajuste_m3, ajuste_m4,
#     ajuste_mon1, ajuste_mon2, ajuste_mon3, ajuste_mon4, ajuste_job1, ajuste_job2,
#     ajuste_job3, ajuste_job4)
# 
# ############
# ## Ajustes com manipulação de meses em forma numérica
# 
# b_mark5 <- data %>% 
#   mutate(month = ifelse(month == "jan", 1, 
#                         ifelse(month == "feb", 2, 
#                                ifelse(month == "mar", 3, 
#                                       ifelse(month == "apr", 4, 
#                                              ifelse(month == 'may', 5, 
#                                                     ifelse(month == "jun", 6, 
#                                                            ifelse(month == "jul", 7, 
#                                                                   ifelse(month == "aug", 8, 
#                                                                          ifelse(month == "sep", 9, 
#                                                                                  ifelse(month == "oct", 10, 
#                                                                                         ifelse(month == "nov", 11, 12)))))))))))) 
# 
# no5 <- which(names(b_mark5) == "previous" | names(b_mark5) == "poutcome")
# 
# b_mark5 <- b_mark5[,-no5]
# 
# ### Vamos separar, aleatoriamente, a base em duas: uma para o ajuste do 
### modelo (com 80% das observações) e outra para validação (com 20% observações).
# mod_obs3 <- ceiling(dim(b_mark3)[1]*0.8)

# set.seed(1234)
# indices3 <- sample(1:dim(b_mark3)[1], size = mod_obs3) 

### indices é um vetor com números de 1 a 4521 numa sequência aleatória.

# bmarkajuste5 <- b_mark5[indices,]
# ### dataframe com as 3617 linhas para ajuste.
# 
# bmarkvalid5 <- b_mark5[-indices,]
# ### dataframe com 904 linhas, apenas para validação.
# 
# ###########################
# 
# ## Ajuste 1
# ajuste_num1 <- glm(y ~ .,family=binomial,data = bmarkajuste5)
# summary(ajuste_num1)
# 
# ## Ajuste 2(metodo backward)
# ajuste_num2 <- step(glm(y ~ .,family=binomial,data = bmarkajuste5), direction = 'backward')
# summary(ajuste_num2)
# 
# ## Ajuste 3(metodo forward)
# ajuste_null_num <- glm(y ~ 1, data = bmarkajuste5, family = 'binomial')
# ajuste_num3 <- step(ajuste_null_num, scope = formula(ajuste_num1), direction = "forward")
# summary(ajuste_num3)
# 
# ## Ajuste 4(metodo stepwise)
# ajuste_num4 <- step(ajuste_null_num, scope = formula(ajuste_num1), direction = "both")
# summary(ajuste_num4)
# 
# AIC(ajuste1, ajuste2, ajuste3, ajuste4, ajuste_m1, ajuste_m2, ajuste_m3, ajuste_m4,
#     ajuste_mon1, ajuste_mon2, ajuste_mon3, ajuste_mon4, ajuste_job1, ajuste_job2,
#     ajuste_job3, ajuste_job4, ajuste_num1, ajuste_num2, ajuste_num3, ajuste_num4)
# 
####################
## Ajustes com manipulação de meses em forma numérica e job

# b_mark6 <- data %>% 
#   mutate(month = ifelse(month == "jan", 1, 
#                         ifelse(month == "feb", 2, 
#                                ifelse(month == "mar", 3, 
#                                       ifelse(month == "apr", 4, 
#                                              ifelse(month == 'may', 5, 
#                                                     ifelse(month == "jun", 6, 
#                                                            ifelse(month == "jul", 7, 
#                                                                   ifelse(month == "aug", 8, 
#                                                                          ifelse(month == "sep", 9, 
#                                                                                 ifelse(month == "oct", 10, 
#                                                                                        ifelse(month == "nov", 11, 12))))))))))),
#          job = ifelse(job %in% active, "Worker", 
#                       ifelse(job %in% inactive, "Inactive",
#                              ifelse(job %in% other, "Other", "self-employed")))) 
# 
# no6 <- which(names(b_mark6) == "previous" | names(b_mark6) == "poutcome")
# 
# b_mark6 <- b_mark6[,-no6]
# 
# ### Vamos separar, aleatoriamente, a base em duas: uma para o ajuste do 
# ### modelo (com 80% das observações) e outra para validação (com 20% observações).
# # mod_obs3 <- ceiling(dim(b_mark3)[1]*0.8)
# 
# # set.seed(1234)
# # indices3 <- sample(1:dim(b_mark3)[1], size = mod_obs3) 
# 
# ### indices é um vetor com números de 1 a 4521 numa sequência aleatória.
# 
# bmarkajuste6 <- b_mark6[indices,]
# ### dataframe com as 3617 linhas para ajuste.
# 
# bmarkvalid6 <- b_mark6[-indices,]
# ### dataframe com 904 linhas, apenas para validação.
# 
# ###########################
# 
# ## Ajuste 1
# ajuste_num_mod1 <- glm(y ~ .,family=binomial,data = bmarkajuste6)
# summary(ajuste_num_mod1)
# 
# ## Ajuste 2(metodo backward)
# ajuste_num_mod2 <- step(glm(y ~ .,family=binomial,data = bmarkajuste6), direction = 'backward')
# summary(ajuste_num_mod2)
# 
# ## Ajuste 3(metodo forward)
# ajuste_null_num_mod <- glm(y ~ 1, data = bmarkajuste6, family = 'binomial')
# ajuste_num_mod3 <- step(ajuste_null_num_mod, scope = formula(ajuste_num_mod1), direction = "forward")
# summary(ajuste_num_mod3)
# 
# ## Ajuste 4(metodo stepwise)
# ajuste_num_mod4 <- step(ajuste_null_num_mod, scope = formula(ajuste_num_mod1), direction = "both")
# summary(ajuste_num_mod4)
# 
# AIC(ajuste1, ajuste2, ajuste3, ajuste4, ajuste_m1, ajuste_m2, ajuste_m3, ajuste_m4,
#     ajuste_mon1, ajuste_mon2, ajuste_mon3, ajuste_mon4, ajuste_job1, ajuste_job2,
#     ajuste_job3, ajuste_job4, ajuste_num1, ajuste_num2, ajuste_num3, ajuste_num4,
#     ajuste_num_mod1, ajuste_num_mod2, ajuste_num_mod3, ajuste_num_mod4)
# 
## Melhor modelo pelo critério AIC foi o ajuste4 com AIC = 1833.575

### Análise de Fresíduos

# plot(ajuste4)

# paj2 <- predict(ajuste4, type = 'response', newdata = bmarkajuste)
# ### Probabilidades estimadas para os indivíduos da base de ajuste;
# 
# pval2 <-predict(ajuste4, type = 'response', newdata = bmarkvalid) 
# ### Probabilidades estimadas para os indivíduos da base de validação.
# 
# round(pval2 * 1000)
# 
################################################################################
### Usando o modelo ajustado para classificação dos clientes da base de validação.

### Como fica se usarmos pc=0,5 como ponto de corte para classificação, ou seja:
# Classificar como no se p < 0,5;
# Classificar como yes se p >= 0,5.

### Vamos cruzar realidade e predição para diferentes pontos de corte.

# classp0.5 <- factor(ifelse(pval2 >= 0.5, 'no', 'yes'))
# classp0.5 ### Classificações usando o ponto de corte 0,5.
# data.frame(pval2, classp0.5) ### Probabilidades estimadas e classificações.
# 
# ### Classificando como "No" indivíduos com p > 0,5.
# tabela1 <- table(classp0.5, bmarkvalid$y) 
# tabela1
# 
# ### Vamos estimar a sensibilidade e a especificidade referentes a esta regra de decisão.
# sensp0.5 <- sum(classp0.5 == 'yes' & bmarkvalid$y == 'yes')/sum(bmarkvalid$y == 'yes')
# sensp0.5
# espec0.5 <- sum(classp0.5 == 'no' & bmarkvalid$y == 'no')/sum(bmarkvalid$y == 'no')
# espec0.5
# 
# ### E se usássemos os dados de ajuste para calcular a sensibilidade e a especificidade?
# 
# classp0.5aj <- factor(ifelse(paj2 >= 0.5, 'no', 'yes'))
# 
# sensp0.5aj <- sum(classp0.5aj == 'yes' & bmarkajuste$y == 'yes')/sum(bmarkajuste$y == 'yes')
# sensp0.5aj
# espec0.5aj <- sum(classp0.5aj == 'no' & bmarkajuste$y == 'no')/sum(bmarkajuste$y == 'no')
# espec0.5aj
# 
# Observe que ao usar os dados de ajuste, tanto a sensibilidade quanto a especificidade
# são maiores. Isso reforça a importância de se usar dados de validação para
# avaliar adequadamente o desempenho preditivo do modelo.

# Observe que ao usar pc = 0,5, temos uma regra de classificação com elevada 
# especificidade (0,98), mas baixa sensibilidade (0,27). Assim, a capacidade
# de se predizer um cliente que não assinará com o banco é alta, mas o poder 
# preditivo para assinantes é baixo.

## Baseado nos dados de ajuste, chegamos a conclusão de usar uma probabilidade > 0.85

# classp0.85 <- factor(ifelse(pval2 >= 0.85, 'no', 'yes'))
# classp0.85 ### Classificações usando o ponto de corte 0,5.
# data.frame(pval2, classp0.85) ### Probabilidades estimadas e classificações.
# 
# ### Classificando como "No" indivíduos com p > 0,5.
# tabela2 <- table(classp0.85, bmarkvalid$y) 
# tabela2
# 
# ### Vamos estimar a sensibilidade e a especificidade referentes a esta regra de decisão.
# sensp0.85 <- sum(classp0.85 == 'yes' & bmarkvalid$y == 'yes')/sum(bmarkvalid$y == 'yes')
# sensp0.85
# espec0.85 <- sum(classp0.85 == 'no' & bmarkvalid$y == 'no')/sum(bmarkvalid$y == 'no')
# espec0.85
# 
# ## Quadro para comparação das probabilidades para sensibilidade e especificidade
# 
# datacomp <- data.frame(c(sensp0.85,sensp0.5),
#                        c(espec0.85,espec0.5))
# names(datacomp)<-c('Sensibilidade','Especificidade')
# rownames(datacomp)<-c('pc=0,85','pc=0,5')
# datacomp
# 
# ### Vamos usar o pacote ROCR para calcular algumas medidas de qualidade preditiva.
# 
# pred <- prediction(pval2, bmarkvalid$y)
# # help('performance')
# 
# ### Vamos plotar a curva ROC
# perf <- performance(pred, measure = "tpr" , x.measure = "fpr") 
# # tpr: True Positive Rate; fpr: False Positive Rate.
# 
# plot(perf, colorize=TRUE,  print.cutoffs.at=seq(0.05,0.95,0.05), lwd = 2)
# abline(0,1, lty = 2)
# 
# performance(pred, 'auc')
# # Extraindo a área sob a curva.
# 
# ### Como ficaria a curva ROC se usássemos os dados de ajuste para sua construção?
# 
# predaj <- prediction(paj2, dadosajuste$Status)
# perfaj <- performance(predaj, measure = "tpr" , x.measure = "fpr") 
# plot(perfaj, col = 'red', lwd = 2, add = T)
# performance(predaj, 'auc')
# 
# ### Tabela de confusão e medidas de performance preditiva.
# ## Metodo manual
# # class <- ifelse(predp > 0.5, 'Yes', 'No')
# # tabcruz <- table(class, dadosvalid$num)
# # tabcruz
# # acur <- sum(diag(tabcruz))/sum(tabcruz); acur ### Acurácia.
# # sens <- tabcruz[2,2]/(tabcruz[1,2] + tabcruz[2,2]); sens ### Sensibilidade.
# # espc <- tabcruz[1,1]/(tabcruz[1,1] + tabcruz[2,1]); espc ### Especificidade.
# ## Metodo automatico
# tabcruz_auto <- confusionMatrix(classp0.85,bmarkvalid$y);tabcruz_auto
# res.roc <- roc(bmarkvalid$y, pval2)
# 
# hnp(ajuste4, main="Envelopes Simulados")


## Tentar com outras funções de ligação
ajuste_null_prob <- glm(y ~ 1, data = bmarkajuste, family = 'binomial'(link = probit))
ajuste_null_clog <- glm(y ~ 1, data = bmarkajuste, family = 'binomial'(link = cloglog))
ajuste_null_cauc <- glm(y ~ 1, data = bmarkajuste, family = 'binomial'(link = cauchit))

ajuste1_prob <- glm(y ~ .,family=binomial(link = probit),data = bmarkajuste)
ajuste1_clog <- glm(y ~ .,family=binomial(link = cloglog),data = bmarkajuste)
ajuste1_cauc <- glm(y ~ .,family=binomial(link = cauchit),data = bmarkajuste)


ajuste_probito <- step(ajuste_null_prob, scope = formula(ajuste1_prob), direction = 'both') 
ajuste_cloglog <- step(ajuste_null_clog, scope = formula(ajuste1_clog), direction = 'both')
ajuste_cauchit <- step(ajuste_null_cauc, scope = formula(ajuste1_cauc), direction = 'both') 
AIC(ajuste4,ajuste_probito,ajuste_cloglog,ajuste_cauchit)

#plot(ajuste_probito)

paj3 <- predict(ajuste_probito, type = 'response', newdata = bmarkajuste)
### Probabilidades estimadas para os indivíduos da base de ajuste;

pval3 <-predict(ajuste_probito, type = 'response', newdata = bmarkvalid) 
### Probabilidades estimadas para os indivíduos da base de validação.

# round(pval3 * 1000)

### Vamos cruzar realidade e predição para diferentes pontos de corte.

classp0.5 <- factor(ifelse(pval3 >= 0.5, 'no', 'yes'))
# classp0.5 ### Classificações usando o ponto de corte 0,5.
data.frame(pval3, classp0.5) ### Probabilidades estimadas e classificações.

### Classificando como "No" indivíduos com p > 0,5.
tabela2 <- table(classp0.5, bmarkvalid$y) 
tabela2

### Vamos estimar a sensibilidade e a especificidade referentes a esta regra de decisão.
sensp0.5 <- sum(classp0.5 == 'no' & bmarkvalid$y == 'no')/sum(bmarkvalid$y == 'no')
sensp0.5
espec0.5 <- sum(classp0.5 == 'yes' & bmarkvalid$y == 'yes')/sum(bmarkvalid$y == 'yes')
espec0.5

### E se usássemos os dados de ajuste para calcular a sensibilidade e a especificidade?

# classp0.5aj <- factor(ifelse(paj3 >= 0.5, 'no', 'yes'))
# 
# sensp0.5aj <- sum(classp0.5aj == 'yes' & bmarkajuste$y == 'yes')/sum(bmarkajuste$y == 'yes')
# sensp0.5aj
# espec0.5aj <- sum(classp0.5aj == 'no' & bmarkajuste$y == 'no')/sum(bmarkajuste$y == 'no')
# espec0.5aj

# Observe que ao usar os dados de ajuste, tanto a sensibilidade quanto a especificidade
# são maiores. Isso reforça a importância de se usar dados de validação para
# avaliar adequadamente o desempenho preditivo do modelo.

# Observe que ao usar pc = 0,5, temos uma regra de classificação com elevada 
# especificidade (0,98), mas baixa sensibilidade (0,27). Assim, a capacidade
# de se predizer um cliente que não assinará com o banco é alta, mas o poder 
# preditivo para assinantes é baixo.

## Baseado nos dados de ajuste, chegamos a conclusão de usar uma probabilidade > 0.89

classp0.89 <- factor(ifelse(pval3 >= 0.89, 'no', 'yes'))
# classp0.89 ### Classificações usando o ponto de corte 0,89.
data.frame(pval3, classp0.89) ### Probabilidades estimadas e classificações.

### Classificando como "No" indivíduos com p > 0,89.
tabela4 <- table(classp0.89, bmarkvalid$y) 
tabela4

### Vamos estimar a sensibilidade e a especificidade referentes a esta regra de decisão.
sensp0.89 <- sum(classp0.89 == 'no' & bmarkvalid$y == 'no')/sum(bmarkvalid$y == 'no')
sensp0.89
espec0.89 <- sum(classp0.89 == 'yes' & bmarkvalid$y == 'yes')/sum(bmarkvalid$y == 'yes')
espec0.89

## Quadro para comparação das probabilidades para sensibilidade e especificidade

datacomp <- data.frame(c(sensp0.89,sensp0.5),
                       c(espec0.89,espec0.5))
names(datacomp)<-c('Sensibilidade','Especificidade')
rownames(datacomp)<-c('pc=0,89','pc=0,5')
datacomp

### Vamos usar o pacote ROCR para calcular algumas medidas de qualidade preditiva.

pred <- prediction(pval3, bmarkvalid$y)
# help('performance')

### Vamos plotar a curva ROC
perf <- performance(pred, measure = "tpr" , x.measure = "fpr") 
# tpr: True Positive Rate; fpr: False Positive Rate.

plot(perf, colorize=TRUE,  print.cutoffs.at=seq(0.05,0.95,0.05), lwd = 2, main="Curva ROC",ylab="Sensibilidade",xlab="Especificidade")
abline(0,1, lty = 2)

performance(pred, 'auc')
# Extraindo a área sob a curva.

### Como ficaria a curva ROC se usássemos os dados de ajuste para sua construção?

predaj <- prediction(paj3, bmarkajuste$y)
perfaj <- performance(predaj, measure = "tpr" , x.measure = "fpr") 
plot(perfaj, col = 'red', lwd = 2, add = T)
performance(predaj, 'auc')

### Tabela de confusão e medidas de performance preditiva.
## Metodo manual
# class <- ifelse(predp > 0.5, 'Yes', 'No')
# tabcruz <- table(class, dadosvalid$num)
# tabcruz
# acur <- sum(diag(tabcruz))/sum(tabcruz); acur ### Acurácia.
# sens <- tabcruz[2,2]/(tabcruz[1,2] + tabcruz[2,2]); sens ### Sensibilidade.
# espc <- tabcruz[1,1]/(tabcruz[1,1] + tabcruz[2,1]); espc ### Especificidade.
## Metodo automatico
tabcruz_auto <- confusionMatrix(classp0.89,bmarkvalid$y);tabcruz_auto
res.roc <- roc(bmarkvalid$y, pval3)

tabcruz_auto2 <- confusionMatrix(classp0.5,bmarkvalid$y);tabcruz_auto2


hnp(ajuste4, main="Envelopes Simulados")
graf_roc <- plot.roc(res.roc, print.auc = TRUE, print.thres = 'best', main="Curva ROC",ylab="Sensibilidade",xlab="Especificidade")


## Usando GAMLLS
m11 <- gamlss(y~pcat(month, method="GAIC", k=2), family = BI(mu.link = "probit"), data=bmarkajuste) # GAIC
### Perguntas

# Interpretação dos parãmetros, cálculo de odds(probito)
# resíduos
# saída da função confusion matrix
# expressar o modelo

### Modelo e seus AICs
##pcat biblioteca gamlss
## gamlss.com flexible regressions
## effect.plot

mn <- gamlss(y~1, family = BI(mu.link = "probit"), data=bmarkajuste) # null model 
ms <- gamlss(y~month-1, family = BI(mu.link = "probit"), data=bmarkajuste) # saturated model 
m1 <- gamlss(y~pcat(month), family = BI(mu.link = "probit"), data=bmarkajuste)
AIC(mn, ms, m1, m11)


groups_m <- unique(getSmo(m11)$coef)
coefs_m <- round(getSmo(m11)$coef, digits = 2)
month_lambda <- round(getSmo(m11)$lambda, digits = 3)
month_tbl <- data.frame(Niveis = levels(bmarkajuste$month), Coeficientes = coefs_m) %>% arrange(Coeficientes)
getSmo(m11)
coef(getSmo(m11))
fitted(getSmo(m11))
plot(getSmo(m11)) # 
# After the fit a new factor is created  this factor has the reduced levels
levels(getSmo(m11)$factor)

m11j <- gamlss(y~pcat(job, method="GAIC", k=2), family = BI(mu.link = "probit"), data=bmarkajuste)
mnj <- gamlss(y~1, family = BI(mu.link = "probit"), data=bmarkajuste) # null model 
msj <- gamlss(y~job-1, family = BI(mu.link = "probit"), data=bmarkajuste) # saturated model 
m1j <- gamlss(y~pcat(job), family = BI(mu.link = "probit"), data=bmarkajuste)
AIC(mnj, msj, m1j, m11j)

coefs_j <- round(getSmo(m11j)$coef, digits = 2)
job_lambda <- round(getSmo(m11j)$lambda, digits = 3)
job_tbl <- data.frame(Niveis = levels(bmarkajuste$job), Coeficientes = coefs_j) %>% arrange(Coeficientes)


getSmo(m11j)
coef(getSmo(m11j))
fitted(getSmo(m11j))
plot(getSmo(m11j)) # 
# After the fit a new factor is created  this factor has the reduced levels
levels(getSmo(m11j)$factor)
plot(getSmo(m11j)$factor)

## Plotando as diferenças

# plotDF(y, factor = month, data = bmarkajuste, family = BI(mu.link = "probit"))
plotLambda(y, factor = month, data = bmarkajuste, family = BI(mu.link = "probit"))
plotLambda(y, factor = job, data = bmarkajuste, family = BI(mu.link = "probit"))

ajustegamll_null <- gamlss(y~1, family = BI(mu.link = "probit"), data=bmarkajuste)
ajustegamll_sat <- gamlss(y~pcat(job, method="GAIC", k=2) + pcat(month, method="GAIC", k=2) +
                                age + marital + education + default + balance + housing + loan +
                                contact + day + duration + campaign + pdays,
                              family = BI(mu.link = "probit"), data=bmarkajuste)
ajustegamll <- step(ajustegamll_null, scope = formula(ajustegamll_sat), direction = 'both')
summary(ajustegamll)
AIC(ajustegamll_sat, ajustegamll)
df <- data.frame(Modelo = c("GAMLLS(probito)", "GLM(logito)", "GLM(probito)"), 
                 AIC = c(ajuste_probito$aic, ajuste4$aic, ajustegamll$aic),
                 `N° Parâmetros` = c(length(ajustegamll$mu.coefficients), length(ajuste4$coefficients),
                                     length(ajuste_probito$coefficients))) %>% 
  arrange(AIC)

## getSmo(ajuste)$coef # me dá a idéia de como ajustar os níveis em grupo
ajtjob <- gamlss(y~pcat(job, method="GAIC", k=log(dim(bmarkajuste)[1]), lambda = -1) + month +
                            age + marital + education + default + balance + housing + loan +
                            contact + day + duration + campaign + pdays,
                          family = BI(mu.link = "probit"), data=bmarkajuste)
getSmo(ajtjob)$coef

ajtmon <- gamlss(y~pcat(month, method="GAIC", k=log(dim(bmarkajuste)[1])) + job +
                   age + marital + education + default + balance + housing + loan +
                   contact + day + duration + campaign + pdays,
                 family = BI(mu.link = "probit"), data=bmarkajuste)

getSmo(ajtmon)$coef

round(g1$coef, digits = 2)
unique(round(g1$coef, digits = 2))# coeficientes pra saber quais niveis formam os grupos
# Anova(ajustegamll)  

## Agrupando os parãmetros


paj4 <- predict(ajustegamll, type = 'response', newdata = bmarkajuste)
### Probabilidades estimadas para os indivíduos da base de ajuste;



pval4 <-predict(ajustegamll, type = 'response', newdata = bmarkvalid) 
### Probabilidades estimadas para os indivíduos da base de validação.

# round(pval3 * 1000)

### Vamos cruzar realidade e predição para diferentes pontos de corte.

classp0.5n <- factor(ifelse(pval4 >= 0.5, 'no', 'yes'))
# classp0.5 ### Classificações usando o ponto de corte 0,5.
data.frame(pval4, classp0.5) ### Probabilidades estimadas e classificações.

### Classificando como "No" indivíduos com p > 0,5.
tabela3 <- table(classp0.5n, bmarkvalid$y) 
tabela3

### Vamos estimar a sensibilidade e a especificidade referentes a esta regra de decisão.
sensp0.5n <- sum(classp0.5n == 'no' & bmarkvalid$y == 'no')/sum(bmarkvalid$y == 'no')
sensp0.5n
espec0.5n<- sum(classp0.5n == 'yes' & bmarkvalid$y == 'yes')/sum(bmarkvalid$y == 'yes')
espec0.5n

### E se usássemos os dados de ajuste para calcular a sensibilidade e a especificidade?

# classp0.5aj <- factor(ifelse(paj3 >= 0.5, 'no', 'yes'))
# 
# sensp0.5aj <- sum(classp0.5aj == 'yes' & bmarkajuste$y == 'yes')/sum(bmarkajuste$y == 'yes')
# sensp0.5aj
# espec0.5aj <- sum(classp0.5aj == 'no' & bmarkajuste$y == 'no')/sum(bmarkajuste$y == 'no')
# espec0.5aj

# Observe que ao usar os dados de ajuste, tanto a sensibilidade quanto a especificidade
# são maiores. Isso reforça a importância de se usar dados de validação para
# avaliar adequadamente o desempenho preditivo do modelo.

# Observe que ao usar pc = 0,5, temos uma regra de classificação com elevada 
# especificidade (0,98), mas baixa sensibilidade (0,27). Assim, a capacidade
# de se predizer um cliente que não assinará com o banco é alta, mas o poder 
# preditivo para assinantes é baixo.

## Baseado nos dados de ajuste, chegamos a conclusão de usar uma probabilidade > 0.89

classp0.89n <- factor(ifelse(pval4 >= 0.89, 'no', 'yes'))
# classp0.89 ### Classificações usando o ponto de corte 0,89.
data.frame(pval4, classp0.89n) ### Probabilidades estimadas e classificações.

### Classificando como "No" indivíduos com p > 0,89.
tabela5 <- table(classp0.89n, bmarkvalid$y) 
tabela5

### Vamos estimar a sensibilidade e a especificidade referentes a esta regra de decisão.
sensp0.89n <- sum(classp0.89n == 'no' & bmarkvalid$y == 'no')/sum(bmarkvalid$y == 'no')
sensp0.89n
espec0.89n <- sum(classp0.89n == 'yes' & bmarkvalid$y == 'yes')/sum(bmarkvalid$y == 'yes')
espec0.89n

## Quadro para comparação das probabilidades para sensibilidade e especificidade

datacomp <- data.frame(c(sensp0.89n,sensp0.5n),
                       c(espec0.89n,espec0.5n))
names(datacomp)<-c('Sensibilidade','Especificidade')
rownames(datacomp)<-c('pc=0,89','pc=0,5')
datacomp

### Vamos usar o pacote ROCR para calcular algumas medidas de qualidade preditiva.

pred2 <- prediction(pval4, bmarkvalid$y)
# help('performance')

### Vamos plotar a curva ROC
perf2 <- performance(pred2, measure = "tpr" , x.measure = "fpr") 
# tpr: True Positive Rate; fpr: False Positive Rate.

plot(perf2, colorize=TRUE,  print.cutoffs.at=seq(0.05,0.95,0.05), lwd = 2, main="Curva ROC",ylab="Sensibilidade",xlab="Especificidade")
abline(0,1, lty = 2)

performance(pred2, 'auc')
# Extraindo a área sob a curva.

### Como ficaria a curva ROC se usássemos os dados de ajuste para sua construção?

predaj2 <- prediction(paj4, bmarkajuste$y)
perfaj2 <- performance(predaj2, measure = "tpr" , x.measure = "fpr") 
plot(perfaj2, col = 'red', lwd = 2, add = T)
performance(predaj2, 'auc')



### Tabela de confusão e medidas de performance preditiva.
## Metodo manual
# class <- ifelse(predp > 0.5, 'Yes', 'No')
# tabcruz <- table(class, dadosvalid$num)
# tabcruz
# acur <- sum(diag(tabcruz))/sum(tabcruz); acur ### Acurácia.
# sens <- tabcruz[2,2]/(tabcruz[1,2] + tabcruz[2,2]); sens ### Sensibilidade.
# espc <- tabcruz[1,1]/(tabcruz[1,1] + tabcruz[2,1]); espc ### Especificidade.
## Metodo automatico
tabcruz_auto_ <- confusionMatrix(classp0.89n,bmarkvalid$y);tabcruz_auto_
res.roc2 <- roc(bmarkvalid$y, pval4)

tabcruz_auto_2 <- confusionMatrix(classp0.5,bmarkvalid$y);tabcruz_auto_2


hnp(ajuste_probito_n, main="Envelopes Simulados")

####### Tablelas do trabalho ############

AIC_ <- data.frame(AIC(ajuste4,ajuste_probito,ajuste_cloglog,ajuste_cauchit)) %>% 
  mutate(Lig = c("Logito", "Probito", "Clog-log", "Cauchit"),
         `N° de Parâmetros` = c(length(ajuste4$coefficients), length(ajuste_probito$coefficients),
                                length(ajuste_cloglog$coefficients), length(ajuste_cauchit$coefficients))) %>% 
  arrange(AIC) %>% 
  dplyr::select(`Função de Ligação` = Lig, AIC, `N° de Parâmetros`)


T_1 <- kableExtra::kable(AIC_, align = 'ccc', booktabs = TRUE) %>% 
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))



