### Vamos utilizar uma aplicação bastante simples de glm, com apenas n=15 observações, 
### para ilustrar o uso de simulação (bootstrap não paramétrico) para
### estimar o viés e o erro padrão das estimativas, bem como obter intervalos 
### de confiança para os parâmetros do modelo. 

### Vamos usar a função Boot do pacote car para realização das simulações. 
### Antes de usar a função, vamos usar o R para fazer "na mão" um ou 
### dois passos do algoritmo, para que você entenda o que é feito.

########################################################################
### Passo 1 - Entrada dos dados e ajuste de um MLG Poisson.

x <- c(1.0, 1.1,1.4, 0.9, 1.6, 0.6, 0.5, 0.6, 0.4, 0.4, 1.4, 0.6, 1.9, 1.0, 0.8)
### Variável explicativa.

y <- c(2, 3, 5, 6, 11,  2,  4,  0,  2,  0,  4,  4,  5, 3, 4) 
### Variável resposta, discreta com valores positivos. Vamos usar o modelo
### de Poisson.

plot(x, y, pch = 20)

dados <- data.frame(x,y)

ajuste <- glm(y ~ x, family = 'poisson', data = dados)
summary(ajuste) 
### Observe as estimativas, os erros padrões assintóticos 
### e os testes para as hipóteses de nulidade dos parâmetros do modelo.

confint(ajuste) ### Intervalos com nível (assintótico) de 95% de confiança.
confint.default(ajuste)
########################################################################
### Passo 2 - Primeiros passos do bootstrap não paramétrico (à mão, para ilustração)

indices <- sample(1:15, replace = T); indices 
### Armazenamos em indices os índices das observações que vão compor a 
### primeira re-amostra. Observe que algumas observações vão aparecer 
### mais de uma vez,outras não aparecerão na reamostra.

dadosb1 <- dados[indices,]; dadosb1 
### dadosb1 é a base de dados correspondente à primeira reamostra. Vamos 
### ajustar o modelo de Poisson para essa primeira reamostra.

ajusteb1 <- glm(y ~ x,family = 'poisson', data = dadosb1)
coef(ajuste) 
### Estimativas dos betas geradas pelo modelo ajustado com a amostra original...
coef(ajusteb1) 
### Estimativas dos betas geradas pelo modelo ajustado 

### Com a primeira reamostra bootstrap. Você pode observar que as estimativas 
### são diferentes daquelas obtidas no primeiro ajuste, o que já era de 
### se esperar (a base é outra).

summary(ajusteb1) ### Resumo do modelo ajustado com a primeira amostra bootstrap.

### Fazendo uma segunda vez:

indices2 <- sample(1:15, replace = T); indices2 ### Outro conjunto de índices.
dadosb2 <- dados[indices2,]; dadosb2 
### A base correspondente aos índices selecionados (nossa segunda reamostra).
ajusteb2 <- glm(y ~ x, family = 'poisson', data = dadosb2)
coef(ajusteb2) ### E as estimativas dos parâmetros geradas pela segunda reamostra.

### A ideia é repetir esses passos um grande número de vezes (gerando um 
### grande número de estimativas) e estimar os parâmetros com base na distribuição
### das estimativas geradas pelas reamostras, conforme discutido em sala de aula.
### Fica como exercício para os alunos programar esse procedimento sem usar alguma 
### função específica do R.

########################################################################
### Passo 3 - Vamos usar a função Boot, do pacote car, para a execução do bootstrap.

require(car)
require(boot)
help(Boot)
b1 <- Boot(ajuste, R = 999) ### Vamos utilizar 999 reamostragens.
head(boot.array(b1)) 
### Com a função boot.array podemos verificar a composição (os índices)  
### em cada reamostra. Observar que cada linha se refere a uma reamostra.

hist(b1) ### Histogramas para as estimativas dos dois parâmetros.
summary(b1) ### Por colunas: Número de reamostragens; 
### Estimativas geradas pelo modelo inicial (com a base original);
### Estimativas bootstrap para o vício; Estimativas bootstrap para
### o erro padrão; Mediana das estimativas bootstrap.
### Comparar as duas últimas colunas com os resultados correspondentes 
### no summary do modelo original.

round(summary(ajuste)$coefficients[,1:2],4)

confint(b1)
confint(b1, type='perc')
confint.default(ajuste) 
### Intervalos de confiança usando bootstrap (de duas formas diferentes)  
### e os intervalos assintóticos. Repare que os intervalos assintóticos são, 
### indevidamente, mais precisos.

### Agora, usando Bootstrap para estimar a resposta esperada para x = 1,5.
### exp(beta0 + beta1 * 1,5).

coef(ajuste)
f <- function(obj) exp(coef(obj)[1] + coef(obj)[2] * 1.5)
bpred <- Boot(ajuste,f , R=999)
confint(bpred, type = 'perc')

p1 <- predict(ajuste, newdata = data.frame(x = 1.5), type = 'link', se.fit = T)
intLink <- c(p1$fit - 1.96 * p1$se.fit, p1$fit + 1.96 * p1$se.fit)
intResp <- exp(intLink)
intResp

### Exercício - Usar bootstrap para produzir inferências para o problema dos
### sinistros (estimar os erros padrões, calcular ICs). Comparar com os 
### correspondentes resultados assintóticos.