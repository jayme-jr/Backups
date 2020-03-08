library(tidyverse)
library(drc)
library(MASS)
library(grid)
library(gridExtra)

########### Análise contínua ############
cont <- read.csv2("base_continua.csv", header = TRUE, sep = ";", dec = ",")

### O ajuste de diferentes modelos e resultados que permitam comparar seus ajustes----------

### Resposta vs dose por grupo.
### Visualização dos resultados
ggplot(cont, aes(x = Dose, y = Resposta)) + geom_point(aes(colour = Grupo))

ggplot(cont, aes(x = I(Dose), y = Resposta)) + geom_point(aes(colour = Grupo)) + facet_grid(~Grupo)
### Apresentando os resultados em janelas separadas.

ggplot(cont, aes(x = I(log(Dose)), y = Resposta)) + geom_point(aes(colour = Grupo))
### Considerando a dose na escala logarítmica, para melhor visualização.

ggplot(cont, aes(x = I(log(Dose)), y = Resposta)) + geom_point(aes(colour = Grupo)) + facet_grid(~Grupo)
### Apresentando os resultados em janelas separadas.

####
### Ajuste dos modelos

### Modelo log-logístico de três parâmetros.
ll_3 <- drm(Resposta ~ Dose, curveid = Grupo, data = cont, fct = LL.3())
summary(ll_3)

### Modelo log-logístico de quatro parâmetros.
ll_4 <- drm(Resposta ~ Dose, curveid = Grupo, data = cont, fct = LL.4())
summary(ll_4)

### Modelo log-logístico de cinco parâmetros.
ll_5 <- drm(Resposta ~ Dose, curveid = Grupo, data = cont, fct = LL.5())
summary(ll_5)

### Modelo log-normal de três parâmetros.
ln_3 <- drm(Resposta ~ Dose, curveid = Grupo, data = cont, fct = LN.3())
summary(ln_3)

### Modelo log-normal de quatro parâmetros.
ln_4 <- drm(Resposta ~ Dose, curveid = Grupo, data = cont, fct = LN.4())
summary(ln_4)

### Modelo Weibull de três parâmetros.
wbl_3 <- drm(Resposta ~ Dose, curveid = Grupo, data = cont, fct = W1.3())
summary(wbl_3)

### Modelo Weibull de quatro parâmetros.
wbl_4 <- drm(Resposta ~ Dose, curveid = Grupo, data = cont, fct = W1.4())
summary(wbl_4)

## O erro padrão evidencia um melhor ajuste no modelo Weibull de quatro parâmetros.
## Mas vamos comparar os modelos com base no coeficiente de Akaik.
AIC(ll_3, ll_4, ll_5, ln_3, ln_4, wbl_3, wbl_4)
## O modelo selecionado para seguir com a análise foi o 
## Weibull de quatro parâmetros com AIC = 221.1363

### Resultados que permitam as curvas de dose-resposta nos diferentes grupos.---------------
### Gráficos dos resultados experimentais com as curvas de dose-resposta ajustadas.
### Representações gráficas do ajuste.
plot(wbl_4) ### Curva ajustada sobre os as médias, em cada dose.
title("Curva Ajustada Sobre as Médias")
plot(wbl_4, type = 'all') ### Curva ajustada sobre os dados amostrais.
title("Curva Ajustada Sobre os Dados Amostrais")

### Vamos investigar se podemos ter um mesmo 'b' para as duas curvas.
wbl_4_b <- drm(Resposta ~ Dose, curveid = Grupo, data = cont, 
               pmodels = data.frame(b = 1, c = Grupo, d = Grupo, e = Grupo), 
               fct = W1.4())
anova(wbl_4, wbl_4_b)
### Não há diferença significativa no ajuste dos dois modelos. Ficamos
### com o modelo mais simples, em que b é o mesmo nas duas curvas.

plot(wbl_4_b)
wbl_4_bs <- summary(wbl_4_b)
summary(wbl_4_b)
AIC(wbl_4_b)
### Nota-se que o AIC diminuiu mais.
### Vamos investigar agora se podemos também fixar um mesmo 'd' para as duas curvas.
wbl_4_bd <- drm(Resposta ~ Dose, curveid = Grupo, data = cont, 
                pmodels = data.frame(b = 1, c = Grupo, d = 1, e = Grupo), 
                fct = W1.4())
anova(wbl_4_b, wbl_4_bd)
### Neste caso, a diferença dos dois ajustes é significativa. Logo, não podemos
### usar o mesmo 'd' nas duas curvas de dose-resposta.

### As equações dos modelos ajustados e a interpretação dos parâmetros------

## ?????????????????????

### Um quadro resumo com as estimativas dos parâmetros, erros padrões e ICs (95%)------
coe <- wbl_4_b$coefficients
tbl_cont <- data.frame(confint(wbl_4_b)) %>% 
  mutate(LI = round(X2.5.., digits = 3),
         LS = round(X97.5.., digits = 3),
         Coeficientes = round(coe, digits = 3),
         `Erro Padrão` = round(wbl_4_bs$coefficients[,2], digits = 3),
         `Parâmetros` = c("b:(Intercept)", "c:a", "c:b", "d:a", "d:b", "e:a", "e:b")) %>% 
  dplyr::select(`Parâmetros`, LI, Coeficientes, LS, `Erro Padrão`) 

View(tbl_cont)
# Para usar select() como pacote MASS carregado, usar dplyr::select 
  
### A análise dos resíduos e o teste da falta de ajuste----------

par(mfrow = c(1,2))
plot(residuals(wbl_4_b, type = 'standard') ~ fitted(wbl_4_b), cex = 1.2, pch = 20,
     xlab = 'Valores ajustados', ylab = 'Resíduos')
qqnorm(residuals(wbl_4_b, type = 'standard'), pch = 20, cex = 1.2)
qqline(residuals(wbl_4_b, type = 'standard'))
### O ajuste ficou satisfatório dados o gráfico de resíduos vs valores 
### ajustados que não mostrou padrões e o qqplot evidenciou normalidade.
### dos resíduos.

### Predições para a resposta para ao menos três doses não consideradas no experimento--------

data_predict <- data.frame(Dose = c(5, 12, 18))
data_predict
### Doses para as quais vamos estimar a probabilidade de resposta.

predict(wbl_4_b, newdata = data_predict, type = 'response', se.fit = FALSE)
### O argumento type = "response" vai garantir que as predições sejam
### as probabilidades de resposta (e não os logitos). O argumento se.fit = TRUE
### garante que, além das probabilidades estimadas, a função retorne os
### respectivos erros padrões.

### Estimativas de doses efetivas-----------
dose_efec_cont <- ED(wbl_4_b, c(5, 10, 50), interval = "delta")
dose_efec_cont
# Gráfico?

Estimate_cont <- dose_efec_cont[,1]; Estimate_cont
EP_cont <- dose_efec_cont[,2]; EP_cont

ICs_cont <- data.frame(LI = Estimate -1.96 * EP_cont, Estimate_cont,  LS = Estimate +1.96 * EP_cont)
ICs_cont
### Estimativas de potências relativas-------------------------
### Vamos calcular a potência relativa. Primeiro para o modelo final, em
### que o parâmetro de inclinação relativa é o mesmo para as duas curvas.
EDcomp(wbl_4_b, percVec = c(50,50)) ### PR(50)
EDcomp(wbl_4_b, percVec = c(90,90)) ### PR(90)
### O fato da inclinação relativa ser a mesma para as duas curvas implica
### que a potência relativa é a mesma, para qualquer dose efetiva.

### Vamos retomar o modelo em que as duas curvas têm inclinações diferentes.
EDcomp(wbl_4, percVec = c(50,50))
EDcomp(wbl_4, percVec = c(90,90))
### Observe que agora a potência relativa varia conforme a dose efetiva considerada.



########### Análise binária ############
bin <- read.csv2("base_binaria.csv", header = TRUE, sep = ";", dec = ",")

### O ajuste de diferentes modelos e resultados que permitam comparar seus ajustes----------
bin$prop <- (bin$resposta+0.5)/(bin$n+1) ### Proporção de respostas.
### As constantes são adicionadas apenas para evitar proporções iguais a 
### zero ou iguais a um, para que seja possível calcular os logitos (isso
### só é necessário na produção dos gráficos).

bin$logito <- log((bin$prop)/(1-bin$prop))
### Cálculo dos logitos.

bin$logDose <- log(bin$dose + 0.5)
### Como alternativa às doses na escala original, vamos considerar também
### as log-doses.

########################################################################
### Análise exploratória.

### Gráficos para a proporção de insetos mortos vs dose (com as doses nas
### escalas original e logaritmica)

p <- ggplot(bin, aes(x = dose, y = prop)) + geom_point(size = 4) +
  geom_line() + xlab('Dose') + ylab('Proporção de Resposta')
q <- ggplot(bin, aes(x = logDose, y = prop)) + geom_point(size = 4) +
  geom_line() + xlab('log(Dose)') + ylab('Proporção de Resposta')

grid.arrange(p, q ,r, s, ncol=2, nrow=2)

### Gráficos para o logito da proporção de respostas vs dose (com as doses nas
### escalas original e logaritmica)
r <- ggplot(bin, aes(x = dose, y = logito)) + geom_point(size = 4) +
  geom_line() + xlab('Dose') + ylab('log(p/(1-p))')

s <- ggplot(bin, aes(x = logDose, y = logito)) + geom_point(size = 4) +
  geom_line() + xlab('log(Dose)') + ylab('log(p/(1-p))')
### Nenhuma das relações se mostraram lineares. 

########################################################################
### Ajuste dos modelos 

### Vamos ajustar oito modelos resultantes das combinações de: quatro distribuições
### para a resistência (logística, normal, Gumbel e Cauchy), e as concentrações
### na escala original e logaritmica.

## Escala original
mod_logis_orig <- glm(prop ~ dose, family = binomial(link = 'logit'), 
                      weights = n, data = bin)
x <-summary(mod_logis_orig) 

T_1_exp <- kableExtra::kable(x$coefficients, align = 'cccccc', booktabs = TRUE,
                             caption = "Base Binária") %>% 
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))

mod_norm_orig <- glm(prop ~ dose, family = binomial(link = 'probit'), 
                     weights = n, data = bin)
summary(mod_norm_orig)

mod_gumb_orig <- glm(prop ~ dose, family = binomial(link = 'cloglog'), 
                     weights = n, data = bin)
summary(mod_gumb_orig)

mod_cauc_orig <- glm(prop ~ dose, family = binomial(link = 'cauchit'), 
                     weights = n, data = bin)
summary(mod_cauc_orig)

## Escala logarítmica
mod_logis_log <- glm(prop ~ logDose, family = binomial(link = 'logit'), 
                     weights = n, data = bin)
summary(mod_logis_log)

mod_norm_log <- glm(prop ~ logDose, family = binomial(link = 'probit'), 
                    weights = n, data = bin)
summary(mod_norm_log)

mod_gumb_log <- glm(prop ~ logDose, family = binomial(link = 'cloglog'), 
                    weights = n, data = bin)
summary(mod_gumb_log)

mod_cauc_log <- glm(prop ~ logDose, family = binomial(link = 'cauchit'), 
                    weights = n, data = bin)
summary(mod_cauc_log)

### Vamos comparar os ajustes usando o critério de informação de Akaike (AIC)
AIC(mod_logis_orig, mod_norm_orig, mod_gumb_orig, mod_cauc_orig, 
    mod_logis_log, mod_norm_log, mod_gumb_log, mod_cauc_log)
### Aqui o m odelo Cauchit(mod_cauc_log) com a dose na escala logarítmica produziu o melhor 
### ajuste (menor AIC).

summary(mod_cauc_log)

### Resultados que permitam as curvas de dose-resposta nos diferentes grupos.---------------
ggplot(bin, aes(x = logDose, y = prop, weight = n)) + geom_point(size = 4) +
  geom_smooth(method = "glm",  se = F, aes(colour="Logito"), 
              method.args = list(family = binomial(link = 'logit'))) +
  geom_smooth(method = "glm",  se = F, aes(colour="Probito"), 
              method.args = list(family = binomial(link = 'probit'))) +
  geom_smooth(method = "glm",  se = F, aes(colour="Cloglog"), 
              method.args = list(family = binomial(link = 'cloglog'))) +
  geom_smooth(method = "glm",  se = F, aes(colour="Cauchy"), 
              method.args = list(family = binomial(link = 'cauchit'))) +
  scale_colour_manual(name="Modelo", values=c("blue", "red", "green", "black"))
  

plot(mod_cauc_log)
### A curva preta representa o ajuste do modelo com função cauchit, que
### produziu menor valor de AIC.

### As equações dos modelos ajustados e a interpretação dos parâmetros------

#????????????????????????????????

### Um quadro resumo com as estimativas dos parâmetros, erros padrões e ICs (95%)------

coe_b <- mod_cauc_log$coefficients
tbl_bin <- data.frame(confint(mod_cauc_log)) %>% 
  mutate(LI = round(X2.5.., digits = 3),
         LS = round(X97.5.., digits = 3),
         Coeficientes = round(coe_b, digits = 3),
         `Erro Padrão` = round(mod_cauc_log$coefficients, digits = 3),
         `Parâmetros` = c("Intercept", "logDose")) %>% 
  dplyr::select(`Parâmetros`, LI, Coeficientes, LS, `Erro Padrão`) 

View(tbl_bin)

### A análise dos resíduos e o teste da falta de ajuste----------

####falta de ajuste
# Para o teste de ajuste, foi utilizado o teste $\tilde{\chi}^2$(Qui-quadrado). Como o modelo teve 
# deviance=76,312 e 8 graus de liberdade, o resultado do teste foi `r aj_bin`, portanto ao nível de 
# significância de 5% não existe evidência significativa de falta de ajuste.

par(mfrow = c(1,2))
plot(residuals(mod_cauc_log, type = 'pearson') ~ fitted(mod_cauc_log), cex = 1.2, pch = 20,
     xlab = 'Valores ajustados', ylab = 'Resíduos')
qqnorm(residuals(mod_cauc_log, type = 'pearson'), pch = 20, cex = 1.2)
qqline(residuals(mod_cauc_log, type = 'pearson'))
### O ajuste ficou satisfatório dados o gráfico de resíduos vs valores 
### ajustados que não mostrou padrões e o qqplot evidenciou normalidade.
### dos resíduos.

### Predições para a resposta para ao menos três doses não consideradas no experimento--------

data_predic <- data.frame(logDose = log(c(0.5, 0.05, 0.005)))
data_predic
### Doses para as quais vamos estimar a probabilidade de resposta.

predict(mod_cauc_log, newdata = data_predic, type = 'response', se.fit = FALSE)
### O argumento type = "response" vai garantir que as predições sejam
### as probabilidades de resposta (e não os logitos). O argumento se.fit = TRUE
### garante que, além das probabilidades estimadas, a função retorne os
### respectivos erros padrões.

### Estimativas de doses efetivas-----------

### Agora, vamos estimar algumas doses efetivas usando a função dose.p, do
### pacote MASS.
dose_efec <- dose.p(mod_cauc_log, cf = c(1,2), p = c(0.25,0.5,0.75))
dose_efec

### O argumento cf indica, no vetor de estimativas, as posições das estimativas
### de beta0 e beta1 que serão consideradas. Veja abaixo:

coef(mod_cauc_log)
### No caso, as estimativas estão nas posições 1 e 2 (só há dois parâmetros
### estimados, no ajuste de uma única curva).

### Voltando, o argumento p indica as doses efetivas que estamos interessados em estimar.

### Se quisermos os intervalos de confiança (95%), basta usar as estimativas
### pontuais e os erros padrões:

Estimate <- c(exp(dose_efec)); Estimate
EP <- attr(dose_efec, "SE"); EP

ICs <- data.frame(LI = Estimate -1.96 * EP, Estimate, LS = Estimate +1.96 * EP)
ICs

### Estimativas de potências relativas-------------------------

### Só é usado para comparação entre grupos(tratamentos) e a base binária não tem
