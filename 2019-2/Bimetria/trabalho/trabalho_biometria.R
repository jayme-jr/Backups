library(tidyverse)
library(drc)
library(MASS)

## Análise binária
bin <- read.csv2("base_binaria.csv", header = TRUE, sep = ";", dec = ",")

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

x11()

### Gráficos para a proporção de insetos mortos vs dose (com as doses nas
### escalas original e logaritmica)
ggplot(bin, aes(x = dose, y = prop)) + geom_point(size = 4) +
  geom_line() + xlab('Dose') + ylab('Proporção de Resposta')
ggplot(bin, aes(x = logDose, y = prop)) + geom_point(size = 4) +
  geom_line() + xlab('log(Dose)') + ylab('Proporção de Resposta')


### Gráficos para o logito da proporção de respostas vs dose (com as doses nas
### escalas original e logaritmica)
ggplot(bin, aes(x = dose, y = logito)) + geom_point(size = 4) +
  geom_line() + xlab('Dose') + ylab('log(p/(1-p))')

ggplot(bin, aes(x = logDose, y = logito)) + geom_point(size = 4) +
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

mod_norm_orig <- glm(prop ~ dose, family = binomial(link = 'probit'), 
                     weights = n, data = bin)
mod_gumb_orig <- glm(prop ~ dose, family = binomial(link = 'cloglog'), 
                     weights = n, data = bin)
mod_cauc_orig <- glm(prop ~ dose, family = binomial(link = 'cauchit'), 
                     weights = n, data = bin)

## Escala logarítmica
mod_logis_log <- glm(prop ~ logDose, family = binomial(link = 'logit'), 
                     weights = n, data = bin)

mod_norm_log <- glm(prop ~ logDose, family = binomial(link = 'probit'), 
                    weights = n, data = bin)
mod_gumb_log <- glm(prop ~ logDose, family = binomial(link = 'cloglog'), 
                    weights = n, data = bin)
mod_cauc_log <- glm(prop ~ logDose, family = binomial(link = 'cauchit'), 
                    weights = n, data = bin)

### Vamos comparar os ajustes usando o critério de informação de Akaike (AIC)
AIC(mod_logis_orig, mod_norm_orig, mod_gumb_orig, mod_cauc_orig, 
    mod_logis_log, mod_norm_log, mod_gumb_log, mod_cauc_log)
### Aqui o m odelo Cauchit(mod_cauc_log) com a dose na escala logarítmica produziu o melhor 
### ajuste (menor AIC).

summary(mod_cauc_log)

ggplot(bin, aes(x = logDose, y = prop, weight = n)) + geom_point(size = 4) +
  geom_smooth(method = "glm",  se = F, 
              method.args = list(family = binomial(link = 'logit'))) +
  geom_smooth(method = "glm",  se = F, colour = 'red',
              method.args = list(family = binomial(link = 'probit'))) +
  geom_smooth(method = "glm",  se = F, colour = 'green',
              method.args = list(family = binomial(link = 'cloglog'))) +
  geom_smooth(method = "glm",  se = F, colour = 'black', 
              method.args = list(family = binomial(link = 'cauchit')))
### A curva preta representa o ajuste do modelo com função cauchit, que
### produziu menor valor de AIC.

### Vamos fazer algumas predições:

data_predic <- data.frame(logDose = log(c(50, 125, 375)))
data_predic
### Doses para as quais vamos estimar a probabilidade de resposta.

predict(mod_cauc_log, newdata = data_predic, type = 'response', se.fit = FALSE)
### O argumento type = "response" vai garantir que as predições sejam
### as probabilidades de resposta (e não os logitos). O argumento se.fit = TRUE
### garante que, além das probabilidades estimadas, a função retorne os
### respectivos erros padrões.

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

ICs <- data.frame(Estimate, LI = Estimate -1.96 * EP, LS = Estimate +1.96 * EP)
ICs

### --------------------
### A título de ilustração, vamos tomar os resultados obtidos para dose=0
### para o ajuste devido à resposta natural.

p0 <- 4/137 ### Proporção de mortos sob conc=0.

selenium_v3 <- selenium_v2[-which(selenium_v2$conc==0),]
### Extraindo da base a primeira linha, referente a conc=0.

selenium_v3$total <- selenium_v3$total*(1-p0)
### Cálculo do número efetivo de indivíduos suscetíveis.

selenium_v3$dead <- selenium_v3$dead - selenium_v3$total*p0
### Cálculo do número efetivo de indivíduos que respondem à substância.
### Da mesma 

selenium_v3$prop <- selenium_v3$dead/selenium_v3$total 
selenium_v3$prop[1] <- 0
selenium_v3$prop[7] <- 1
### Contornando o problema de proporções efetivas fora do intervalo [0,1].

mod_logis_orig_v2 <- glm(prop ~ conc, family = binomial(link = 'logit'), 
                         weights = total, data = selenium_v3)

### vamos comparar os resultados dos ajustes.
summary(mod_logis_orig)
summary(mod_logis_orig_v2)


########################################################################
### Ajuste dos modelos - parte 2.

### Vamos começar pelo modelo mais geral (retas concorrentes) apenas para
### decidir se as doses serão usadas na escala original ou logaritmica, e
### a função de ligação (distribuição para as tolerâncias)

### Nesta primeira parte, vamos considerar os quatro compostos.
mod_logis_orig <- glm(prop ~ conc*type, family = binomial(link = 'logit'), 
                      weights = total, data = selenium)

mod_logis_log <- glm(prop ~ logConc*type, family = binomial(link = 'logit'), 
                     weights = total, data = selenium)

mod_norm_orig <- glm(prop ~ conc*type, family = binomial(link = 'probit'), 
                     weights = total, data = selenium)

mod_norm_log <- glm(prop ~ logConc*type, family = binomial(link = 'probit'), 
                    weights = total, data = selenium)

mod_gumb_orig <- glm(prop ~ conc*type, family = binomial(link = 'cloglog'), 
                     weights = total, data = selenium)

mod_gumb_log <- glm(prop ~ logConc*type, family = binomial(link = 'cloglog'), 
                    weights = total, data = selenium)

mod_cauc_orig <- glm(prop ~ conc*type, family = binomial(link = 'cauchit'), 
                     weights = total, data = selenium)

mod_cauc_log <- glm(prop ~ logConc*type, family = binomial(link = 'cauchit'), 
                    weights = total, data = selenium)

AIC(mod_logis_orig, mod_norm_orig, mod_gumb_orig, mod_cauc_orig, 
    mod_logis_log, mod_norm_log, mod_gumb_log, mod_cauc_log)

### O modelo que produziu melhor ajuste (menor valor de AIC) usa função de
### ligação Cauchit com as doses na escala logaritmica: g(p) = tan(π(p − 1/2)) = b0 + b1*logdose,
### ou seja, p = (arctan(b0 + b1*logdose))/pi + 1/2.

### Resumo do ajuste e curvas ajustadas.
summary(mod_cauc_log)
ggplot(selenium, aes(x = logConc, y = prop, weight = total, col = type)) + geom_point(size = 4) +
  geom_smooth(method = "glm",  se = F, 
              method.args = list(family = binomial(link = 'cauchit')))

### Vamos testar se podemos simplificar o modelo para o de retas paralelas, 
### ou o de retas coincidentes.

mod_cauc_log_par <- glm(prop ~ logConc+type, family = binomial(link = 'cauchit'), 
                        weights = total, data = selenium)

mod_cauc_log_coinc <- glm(prop ~ logConc, family = binomial(link = 'cauchit'), 
                          weights = total, data = selenium)

anova(mod_cauc_log_coinc, mod_cauc_log_par, mod_cauc_log, test = 'Chisq')

### Ambos os testes são significativos, indicando que há um melhor ajuste 
### do modelo de retas concorrentes em relação ao de retas paralelas 
### (Deviance=240.978, p-valor < 2.2e-16), e ainda, o ajuste do modelo
### de retas paralelas se ajusta melhor do que o de retas coincidentes
### (Deviance=73.448, p-valor=7.793e-16). Assim, devemos optar pelo modelo
### de retas concorrentes, sem qualquer simplificação.

### Para estimar as doses efetivas, vamos ajustar o mesmo modelo numa parametrização
### diferente.

mod_cauc_log_si <- glm(prop ~ logConc*type - 1 - logConc, family = binomial(link = 'cauchit'), 
                       weights = total, data = selenium)
mod_cauc_log_si
### Nesta formulação, cada parâmetro do modelo é, diretamente, o intercepto
### ou a inclinação de um dos modelos.

### Para o composto do tipo I:
dose_efec_tp1 <- dose.p(mod_cauc_log_si, cf = c(1,5), p = c(0.25,0.5,0.75))
dose_efec_tp1

Estimate <- c(dose_efec_tp1); Estimate
EP <- attr(dose_efec_tp1, "SE"); EP

ICs_tp1 <- exp(data.frame(Estimate, LI = Estimate -1.96 * EP, LS = Estimate +1.96 * EP))
ICs_tp1

########################################################################
### Análise contínua
cont <- read.csv2("base_continua.csv", header = TRUE, sep = ";", dec = ",")

### Visualização dos resultados
x11()
ggplot(cont, aes(x = Dose, y = Resposta)) + geom_point(aes(colour = Grupo))
### Resposta vs dose por grupo.

ggplot(cont, aes(x = I(log(Dose)), y = Resposta)) + geom_point(aes(colour = Grupo))
### Considerando a dose na escala logarítmica, para melhor visualização.

ggplot(cont, aes(x = I(log(Dose)), y = Resposta)) + geom_point() + facet_grid(~Grupo)
### Apresentando os resultados em janelas separadas.

#######################################################################
### Ajuste dos modelos

### Comecemos pelo modelo log-logístico de quatro parâmetros.
ll_4 <- drm(Resposta ~ Dose, curveid = Grupo, data = cont, fct = LL.4())
S.ll_4 <- summary(ll_4)
S.ll_4
par(mfrow = c(1,2))
plot(residuals(ll_4, type = 'standard') ~ fitted(ll_4), cex = 1.2, pch = 20,
     xlab = 'Valores ajustados', ylab = 'Resíduos')
qqnorm(residuals(ll_4, type = 'standard'), pch = 20, cex = 1.2)
qqline(residuals(ll_4, type = 'standard'))
### O ajuste ficou satisfatório dados o gráfico de resíduos vs valores 
### ajustados que não mostrou padrões e o qqplot evidenciou normalidade.
### dos resíduos.

## Comparação com outros modelos
m_all <- mselect(ll_4, list(LL.5(), W1.3(), W1.4(), W2.4(), LN.3(), LN.4()))
m_all
## O modelo weibull de 4 parâmetros produziu o menos AIC.

wbl_4 <- drm(Resposta ~ Dose, curveid = Grupo, data = cont, 
                    fct = W1.4())
## COmparando o AIC dos dois modelos(log-logística de 4 parâmetros e
## weibull de 4 parâmetros)
AIC(wbl_4, ll_4)

## Corroborando que o modelo weibull de 4 parâmetros se mostrou melhor ajustado.
summary(wbl_4)

### Representações gráficas do ajuste.
plot(wbl_4) ### Curva ajustada sobre os as médias, em cada dose.
plot(wbl_4, type = 'all') ### Curva ajustada sobre os dados amostrais.

### Vamos investigar se podemos ter um mesmo 'b' para as duas curvas.
wbl_4_b <- drm(Resposta ~ Dose, curveid = Grupo, data = cont, 
               pmodels = data.frame(b = 1, c = Grupo, d = Grupo, e = Grupo), 
               fct = W1.4())
anova(wbl_4, wbl_4_b)
### Não há diferença significativa no ajuste dos dois modelos. Ficamos
### com o modelo mais simples, em que b é o mesmo nas duas curvas.

plot(wbl_4_b)
summary(wbl_4_b)

### Vamos investigar agora se podemos também fixar um mesmo 'd' para as duas curvas.
wbl_4_bd <- drm(Resposta ~ Dose, curveid = Grupo, data = cont, 
                pmodels = data.frame(b = 1, c = Grupo, d = 1, e = Grupo), 
                fct = W1.4())
anova(wbl_4_b, wbl_4_bd)
### Neste caso, a diferença dos dois ajustes é significativa. Logo, não podemos
### usar o mesmo 'd' nas duas curvas de dose-resposta.

### Estimação de doses efetivas
ED(wbl_4_b, c(5, 10, 50), interval = "delta")

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

### Vamos usar o método Box-Cox, para encontrar uma transformação que -----
### contorne os problemas de ajuste detectados na análise de resíduos.
### Neste caso, usamos a estratégia de transformar ambos os lados do 
### modelo (tanto a resposta quanto o preditor não linear).

G.aparine.m2 <- boxcox(G.aparine.m1)
G.aparine.m2
### A análise indica uma transformação do tipo potência com lambda=0.25.

### Seguimos com o diagnóstico do ajuste baseado nos resíduos.
par(mfrow = c(1,2))
plot(residuals(G.aparine.m2, type = 'standard') ~ fitted(G.aparine.m1), cex = 1.2, pch = 20)
qqnorm(residuals(G.aparine.m2, type = 'standard'), pch = 20, cex = 1.2)
qqline(residuals(G.aparine.m2, type = 'standard'))
### Evidentemente o modelo está mais bem ajustado que o anterior. 

modelFit(G.aparine.m2)
### Não há evidências de falta de ajuste.

### Vamos comparar os resultados dos dois ajustes (estimativas e erros padrões)
S.aparine.m2 <- summary(G.aparine.m2)
S.aparine.m2

data_result <- data.frame(EstM1 = S.aparine.m1$coefficients[,'Estimate'],
                          SeM1 = S.aparine.m1$coefficients[,'Std. Error'],
                          EstM2 = S.aparine.m2$coefficients[,'Estimate'],
                          SeM2 = S.aparine.m2$coefficients[,'Std. Error'])
data_result
### Pode-se observar variações nas estimativas e, sobretudo, em alguns
### erros padrões.

### Usando a mesma transformação, vamos avaliar os ajustes de outros modelos.
m1 <- mselect(G.aparine.m2, list(LL.4(), LL.5(), W1.3(), W1.4(), W2.4(), LN.3(), LN.4()))
m1
### O modelo Weibull de quatro parâmetros produziu menor valor de AIC.

G.aparine.m3 <- drm(drymatter ~ dose, curveid = treatment, data = G.aparine, 
                    fct = W1.4(), bcVal = 0.25)
AIC(G.aparine.m3)
summary(G.aparine.m3)
### Vamos seguir a análise com este modelo.

### Representações gráficas do ajuste.
plot(G.aparine.m3) ### Curva ajustada sobre os as médias, em cada dose.
plot(G.aparine.m3, type = 'all') ### Curva ajustada sobre os dados amostrais.

### Vamos investigar se podemos ter um mesmo 'b' para as duas curvas.
G.aparine.m3_b <- drm(drymatter ~ dose, curveid = treatment, data = G.aparine, 
                      pmodels = data.frame(b = 1, c = treatment, d = treatment, e = treatment), 
                      fct = W1.4(), bcVal = 0.25)
anova(G.aparine.m3, G.aparine.m3_b)
### Não há diferença significativa no ajuste dos dois modelos. Ficamos
### com o modelo mais simples, em que b é o mesmo nas duas curvas.

plot(G.aparine.m3_b)
summary(G.aparine.m3_b)

### Vamos investigar agora se podemos também fixar um mesmo 'd' para as duas curvas.
G.aparine.m3_bd <- drm(drymatter ~ dose, curveid = treatment, data = G.aparine, 
                       pmodels = data.frame(b = 1, c = treatment, d = 1, e = treatment), 
                       fct = W1.4(), bcVal = 0.25)
anova(G.aparine.m3_b, G.aparine.m3_bd)
### Neste caso, a diferença dos dois ajustes é significativa. Logo, não podemos
### usar o mesmo 'd' nas duas curvas de dose-resposta.

### Estimação de doses efetivas
ED(G.aparine.m3_b, c(5, 10, 50), interval = "delta")

### Vamos calcular a potência relativa. Primeiro para o modelo final, em
### que o parâmetro de inclinação relativa é o mesmo para as duas curvas.
EDcomp(G.aparine.m3_b, percVec = c(50,50)) ### PR(50)
EDcomp(G.aparine.m3_b, percVec = c(90,90)) ### PR(90)
### O fato da inclinação relativa ser a mesma para as duas curvas implica
### que a potência relativa é a mesma, para qualquer dose efetiva.

### Vamos retomar o modelo em que as duas curvas têm inclinações diferentes.
EDcomp(G.aparine.m3, percVec = c(50,50))
EDcomp(G.aparine.m3, percVec = c(90,90))
### Observe que agora a potência relativa varia conforme a dose efetiva considerada.

### Visualização dos resultados------
x11()
ggplot(bin, aes(x = dose, y = resposta)) + geom_point()
### resposta x dose.

ggplot(bin, aes(x = I(log(dose)), y = resposta)) + geom_point()
### Considerando a dose na escala logarítmica, para melhor visualização.

########################################################################
### Ajuste dos modelos

options(digits = 8)

### Modelo log-logístico de quatro parâmetros.
ll_4 <- drm(resposta ~ dose, data = bin, fct = LL.4())

coef(ll_4) ### Estimativas dos parâmetros.
round(coef(ll_4),5)

summary(ll_4) 
### Resumo do ajuste. nenhum dos parâmetros difere significativamente de zero

### LL.3 (modelo log-logístico de três parâmetros).

ll_3 <- drm(resposta ~ dose, data = bin, fct = LL.3())
summary(ll_3)

### Resumo do ajuste. somente o parâmetro 'd' difere significativamente de zero
### com p-valor = 0.004. ajustar um novo modelo com os outros parâmetros fixados 
### em zero

ll_3_alt <- drm(resposta ~ dose, data = bin, fct = LL.3(fixed = c(0, NA, NA)))
summary(ll_3_alt)

ll_3_alt2 <- drm(resposta ~ dose, data = bin, fct = LL.3(fixed = c(NA, NA, 0)))
summary(ll_3_alt2)

### Agora, vamos avaliar o ajuste do modelo log-logístico com o quinto parâmetro
### (sem fixar nenhum parâmetro.

ll_5 <- drm(resposta ~ dose, data = bin, fct = LL.5())
summary(ll_5)

### Como não se rejeita a hipótese H0: f=0 (p=0.695), o modelo log-logístico 
### de cinco parâmetros não é necessário.

ll_2 <- drm(resposta ~ dose, data = bin, fct = LL2.2())
summary(ll_2)

### Representações gráficas do ajuste.
plot(ll_2) ### Curva ajustada sobre os as médias, em cada dose.
plot(ll_2, type = 'all') ### Curva ajustada sobre os dados amostrais.
plot(ll_2, type="confidence") ### Curva ajustada com as bandas de confiança (95%.
plot(ll_2, type="bars") ### Curva ajustada com os erros padrões.

### Análise de resíduos
residuos <- residuals(ll_2, typeRes = 'standardised')
fit <- fitted(ll_2)

par(mfrow = c(1,2))
plot(residuos ~ fit, cex = 1.2, pch = 20)
qqnorm(residuos, pch = 20, cex = 1.2)
qqline(residuos)
### Há uma observação com indício de mal ajustada.
### mas dado o pequeno número de observações o qqplot
### parece satisfatório.

### Intervalos de confiança(95%) para os parâmetros do modelo.
confint(ll_2)

### Algumas predições. Vamos trabalhar com as seguintes doses: 5, 50 e 150.
new_doses <- data.frame(dose = c(5,50,150)) ### Doses usadas para predição.
predict(ll_2, newdata = new_doses) ### Valores ajustados.
predict(ll_2, se.fit = TRUE, newdata = new_doses) 
### Valores ajustados com os erros padrões.
predict(ll_2, interval = 'confidence', newdata = new_doses) 
### Valores ajustados e ICs(95%) para a resposta média.
predict(ll_2, interval = 'prediction', newdata = new_doses) 
### Valores ajustados e ICs(95%) para a predição de nova observação.

### Estimação de doses efetivas. Vamos estimar as doses para as seguintes
### respostas (relativas à resposta maximal): 5, 50 e 90.
ED(ll_2, c(5, 10, 50), interval = "delta")

### Teste da falta de ajuste
modelFit(ll_2)
### A hipótese nula, de que o modelo se ajusta bem aos dados, não é rejeitada. 

### Vamos ajustar e avaliar outros modelos.

### Modelos Weibull de três e quatro parâmetros
terb_wei_m3 <- drm(rgr ~ dose, data = terbuthylazin, fct = W1.3())
terb_wei_m4 <- drm(rgr ~ dose, data = terbuthylazin, fct = W1.4())

### Modelos Log-Normal de três e quatro parâmetros
terb_ln_m3 <- drm(rgr ~ dose, data = terbuthylazin, fct = LN.3())
terb_ln_m4 <- drm(rgr ~ dose, data = terbuthylazin, fct = LN.4())

### Vamos comparar os ajustes usando os critérios de informação.

AIC(terb_m2, terb_wei_m3, terb_wei_m4, terb_ln_m3, terb_ln_m4)
### O modelo log-normal de três parâmetros produziu menor valor de AIC
### (melhor ajuste), seguido pelo modelo log-logístico de três parâmetros.

BIC(terb_m2, terb_wei_m3, terb_wei_m4, terb_ln_m3, terb_ln_m4)
### A conclusão para o índice BIC, neste caso, é semelhante à produzida 
### pelo AIC.

### Podemos ajustar e comparar vários modelos, de maneira alternativa, usando
### a função mselect.
mselect(terb_m2, list(LL.4(), LL.5(), W1.3(), W1.4(), W2.4(), LN.3(), LN.4()))
### O modelo log-normal de três parâmetros produziu melhores indicadores.

### Voltando ao modelo log-logístico de três parâmetros, vamos verificar a
### necessidade de transformação dos dados usando o método de Box-Cox.

terb_m2_bc <- boxcox(drm(rgr ~ dose, data = terbuthylazin, fct = LL.4(fixed = c(NA, 0, NA, NA))))
### O gráfico da log-verossimilhança perfilada indica que não há a necessidade
### de transformação. Na próxima aplicação, uma transformação será requerida. 


### outro tipo de modelo levando em conta o n

### Visualização dos resultados
x11()
ggplot(bin, aes(x = dose, y = resposta)) + geom_point(aes(colour = n))
### Matéria seca vs dose por tratamento.

ggplot(bin, aes(x = I(log(dose)), y = resposta)) + geom_point(aes(colour = n))
### Considerando a dose na escala logarítmica, para melhor visualização.

ggplot(bin, aes(x = I(log(dose)), y = resposta)) + geom_point() + facet_grid(~n)
### Apresentando os resultados em janelas separadas.

########################################################################
### Ajuste dos modelos

### Comecemos pelo modelo log-logístico de quatro parâmetros.
llbin4 <- drm(n ~ dose, curveid = resposta, data = bin, fct = LL.4())
Sbin4 <- summary(llbin4)
Sbin4
par(mfrow = c(1,2))
plot(residuals(G.aparine.m1, type = 'standard') ~ fitted(G.aparine.m1), cex = 1.2, pch = 20,
     xlab = 'Valores ajustados', ylab = 'Resíduos')
qqnorm(residuals(G.aparine.m1, type = 'standard'), pch = 20, cex = 1.2)
qqline(residuals(G.aparine.m1, type = 'standard'))
### Os gráficos de resíduos claramente apontam problemas no ajuste, com
### variância não homogênea e alguma não normalidade.

### Vamos usar o método Box-Cox, para encontrar uma transformação que 
### contorne os problemas de ajuste detectados na análise de resíduos.
### Neste caso, usamos a estratégia de transformar ambos os lados do 
### modelo (tanto a resposta quanto o preditor não linear).

G.aparine.m2 <- boxcox(G.aparine.m1)
G.aparine.m2
### A análise indica uma transformação do tipo potência com lambda=0.25.

### Seguimos com o diagnóstico do ajuste baseado nos resíduos.
par(mfrow = c(1,2))
plot(residuals(G.aparine.m2, type = 'standard') ~ fitted(G.aparine.m1), cex = 1.2, pch = 20)
qqnorm(residuals(G.aparine.m2, type = 'standard'), pch = 20, cex = 1.2)
qqline(residuals(G.aparine.m2, type = 'standard'))
### Evidentemente o modelo está mais bem ajustado que o anterior. 

modelFit(G.aparine.m2)
### Não há evidências de falta de ajuste.

### Vamos comparar os resultados dos dois ajustes (estimativas e erros padrões)
S.aparine.m2 <- summary(G.aparine.m2)
S.aparine.m2

data_result <- data.frame(EstM1 = S.aparine.m1$coefficients[,'Estimate'],
                          SeM1 = S.aparine.m1$coefficients[,'Std. Error'],
                          EstM2 = S.aparine.m2$coefficients[,'Estimate'],
                          SeM2 = S.aparine.m2$coefficients[,'Std. Error'])
data_result
### Pode-se observar variações nas estimativas e, sobretudo, em alguns
### erros padrões.

### Usando a mesma transformação, vamos avaliar os ajustes de outros modelos.
m1 <- mselect(G.aparine.m2, list(LL.4(), LL.5(), W1.3(), W1.4(), W2.4(), LN.3(), LN.4()))
m1
### O modelo Weibull de quatro parâmetros produziu menor valor de AIC.

G.aparine.m3 <- drm(drymatter ~ dose, curveid = treatment, data = G.aparine, 
                    fct = W1.4(), bcVal = 0.25)
AIC(G.aparine.m3)
summary(G.aparine.m3)
### Vamos seguir a análise com este modelo.

### Representações gráficas do ajuste.
plot(G.aparine.m3) ### Curva ajustada sobre os as médias, em cada dose.
plot(G.aparine.m3, type = 'all') ### Curva ajustada sobre os dados amostrais.

### Vamos investigar se podemos ter um mesmo 'b' para as duas curvas.
G.aparine.m3_b <- drm(drymatter ~ dose, curveid = treatment, data = G.aparine, 
                      pmodels = data.frame(b = 1, c = treatment, d = treatment, e = treatment), 
                      fct = W1.4(), bcVal = 0.25)
anova(G.aparine.m3, G.aparine.m3_b)
### Não há diferença significativa no ajuste dos dois modelos. Ficamos
### com o modelo mais simples, em que b é o mesmo nas duas curvas.

plot(G.aparine.m3_b)
summary(G.aparine.m3_b)

### Vamos investigar agora se podemos também fixar um mesmo 'd' para as duas curvas.
G.aparine.m3_bd <- drm(drymatter ~ dose, curveid = treatment, data = G.aparine, 
                       pmodels = data.frame(b = 1, c = treatment, d = 1, e = treatment), 
                       fct = W1.4(), bcVal = 0.25)
anova(G.aparine.m3_b, G.aparine.m3_bd)
### Neste caso, a diferença dos dois ajustes é significativa. Logo, não podemos
### usar o mesmo 'd' nas duas curvas de dose-resposta.

### Estimação de doses efetivas
ED(G.aparine.m3_b, c(5, 10, 50), interval = "delta")

### Vamos calcular a potência relativa. Primeiro para o modelo final, em
### que o parâmetro de inclinação relativa é o mesmo para as duas curvas.
EDcomp(G.aparine.m3_b, percVec = c(50,50)) ### PR(50)
EDcomp(G.aparine.m3_b, percVec = c(90,90)) ### PR(90)
### O fato da inclinação relativa ser a mesma para as duas curvas implica
### que a potência relativa é a mesma, para qualquer dose efetiva.

### Vamos retomar o modelo em que as duas curvas têm inclinações diferentes.
EDcomp(G.aparine.m3, percVec = c(50,50))
EDcomp(G.aparine.m3, percVec = c(90,90))
### Observe que agora a potência relativa varia conforme a dose efetiva considerada.
