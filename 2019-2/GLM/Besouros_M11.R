### Regressão para dados binários
require(ggplot2)
require(car)

### Exemplo 2
### Dados de mum experimento do tipo dose-resposta, referente à mortalidade 
### de besouros sob três tratamentos (DDT, BHC e DDT+BHC), sob seis diferentes
### doses. 

ldose <- log(rep(c(2, 2.64, 3.48, 4.59, 6.06, 8), 3)) ### log-dose

Trat <- rep(c('DDT', 'BHC', 'DDT+BHC'), each = 6) ### tratamento

Resposta <- c(3, 5, 19, 19, 24, 35, 2, 14, 20, 27, 41, 40, 28, 37, 46, 48, 48, 50)
### Número de insetos mortos sob cada condição experimental.

n <- c(50, 49, 47, 50, 49, 50, 50, 49, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50)
### Número de insetos submetidos a cada condição experimental.

dados <- data.frame(ldose,Trat,Resposta,n)

### Análise descritiva.
x11()
g0 <- ggplot(dados, aes(x = ldose, y = Resposta/n, color=Trat)) +
  geom_point(size = 4) + theme_bw(base_size = 18) + 
  labs(x = 'Dose(log)', y = 'Proporção de insetos mortos') +
  theme(legend.position="top")
g0

################################################################################
### Parte 1 - Vamos começar ajustando um modelo apenas para os insetos 
### tratados com BHC. 

data_BCH <- subset(dados, Trat=='BHC')

g1 <- ggplot(data_BCH, aes(x = ldose, y = Resposta/n)) +
  geom_point(size = 4) + theme_bw(base_size = 18) + 
  labs(x = 'Dose(log)', y = 'Proporção de insetos mortos') +
  theme(legend.position="top")
g1

### Modelo com função de ligação logito.
ajuste1 <- glm(cbind(Resposta, n-Resposta) ~ ldose, family = binomial, 
               data = data_BCH) 
### A primeira forma de declarar a resposta quando temos um glm binomial 
### com dados grupados é por umja matriz com duas colunas, cada uma delas
### com as contagens de um dos desfechos binários (insetos mortos e não
### mortos, no caso).

### Forma alternativa:
ajuste1_2 <- glm(Resposta/n ~ ldose, family = binomial, data = data_BCH, 
                 weights = n) 
### Neste caso, declaramos a resposta como a fração de insetos mortos, e
### o número de insetos mortos é declarado em weights.

summary(ajuste1)
summary(ajuste1_2)
### Os modelos ajustados são rigorosamente os mesmos. Muda apenas a forma de 
### declaração.

### Exercício: Escreva a equação do modelo ajustado e calcule a estimativa 
### da probabilidade de morte para insetos submetidos a uma dose de 7 unidades.
### Adicionalmente, interprete (se couber) os parâmetros do modelo.

### Agora, vamos adicionar ao gráfico a curva referente ao modelo ajustado.
grid_ldose <- seq(log(2), log(8), 0.01) ### grid de valores para a (log) dose.
preditos <- predict(ajuste1, newdata=data.frame(ldose = grid_ldose), type='response')

### Probabilidades estimadas para as doses no grid que criamos.
new.data <- data.frame(grid_ldose, preditos)

g2 <- g1 + geom_line(data = new.data, aes(x = grid_ldose, y = preditos),  
                     size = 1.2, color = 'black')
g2

################################################################################
### Modelo com função de ligação probito
ajuste2 <- glm(Resposta/n ~ ldose, family = binomial(link = 'probit'), 
               data = data_BCH, weights = n)
summary(ajuste2) 
### Exercício: Escreva a equação do modelo ajustado e calcule a estimativa 
### da probabilidade de morte para insetos submetidos a uma dose de 7 unidades.

### Adicionando ao gráfico a curva referente ao odelo ajustado.
preditos2 <- predict(ajuste2, newdata=data.frame(ldose = grid_ldose), type='response')
new.data2 <- data.frame(grid_ldose, preditos2)

g3 <- g2 + geom_line(data = new.data2, aes(x = grid_ldose, y = preditos2),  
                     size = 1.2, color = 'blue')
g3

################################################################################
### Modelo com função de ligação complemento log-log
ajuste3 <- glm(Resposta/n ~ ldose, family = binomial(link = 'cloglog'), 
               data = data_BCH, weights = n)
summary(ajuste3) 
### Exercício: Escreva a equação do modelo ajustado e calcule a estimativa 
### da probabilidade de morte para insetos submetidos a uma dose de 7 unidades.

### Adicionando ao gráfico a curva referente ao odelo ajustado.
preditos3 <- predict(ajuste3, newdata = data.frame(ldose = grid_ldose), type='response')
new.data3 <- data.frame(grid_ldose, preditos3)

g4 <- g3 + geom_line(data = new.data3, aes(x = grid_ldose, y = preditos3),  
                     size = 1.2, color = 'green')
g4


################################################################################
### Vamos comparar os ajustes dos três modelos calculando os respectivos AICs.

AIC(ajuste1, ajuste2, ajuste3)
c(logLik(ajuste1), logLik(ajuste2), logLik(ajuste3)) 
### Log-verossimilhanças maximizadas.

### O modelo com ligação probito é ligeiramente superior ao com ligação logito.
### O modelo cloglog claramente tem pior ajuste que seus concorrentes.

### E se ao invés de analisarmos os dados da forma como estão disponíveis (grupados)
### analisarmos na forma não grupada (ou seja, um inseto por linha)? Vamos construir
### uma nova base.

data_BCH2 <- data_BCH[rep(seq_len(nrow(data_BCH)), times=data_BCH$n),1:2]
respostas <- c(rbind(data_BCH$Resposta, data_BCH$n - data_BCH$Resposta)) 
data_BCH2$resp_bin <- rep(c(1,0,1,0,1,0,1,0,1,0,1,0), times = respostas)
head(data_BCH2, 20)
### data_BCH2 é a base de dados desagregados. Agora, cada linha da base é o
### dado de um único besouro, e a resposta é 1, para os insetos mortos, e zero,
### para os vivos.

### Vamos ajustar o modelo de regressão binomial com ligação logito à base
### de dados desagregada.

ajuste1_desag <- glm(resp_bin ~ ldose, family = binomial, data = data_BCH2) 
summary(ajuste1_desag)

### E vamos comparar os ajustes dos modelos aplicados à base agregada e à base
### não agregada por meio das estimativas dos parâmetros e correspondentes 
### erros padrões.

compareCoefs(ajuste1, ajuste1_desag)
### Observe que o modelo ajustado às duas bases é idêntico. Então, usar a base de
### dados agregada ou não agregada não altera o resultado do ajuste.

logLik(ajuste1)
logLik(ajuste1_desag)
### As escalas das verossimilhanças, no entanto, são diferentes. Em particular,
### o teste da qualidade do ajuste, baseado na deviance residual, se aplica aos
### dados agregados (para n grande), mas não aos dados desagregados. Vamos avaliar
### a qualidade do ajuste.

pchisq(deviance(ajuste1), df = ajuste1$df.residual, lower.tail = FALSE)
### p-valor do teste da qualidade do ajuste. Ao nível de significância de 5%
### não temos evidência de que o modelo não se ajuste bem aos dados.

### Exercício - Avaliar a qualidade dos ajustes com base nas deviances 
### (teste ao nível de significância de 5%). Faça também qqplots com envelopes simulados.


################################################################################
################################################################################
################################################################################
### Parte 2 - Agora, vamos analisar os três tratamentos conjuntamente. Nesta  
### etapa, vamos usar apenas a função de ligação logito

### Gráfico de dispersão.
x11()
h0 <- ggplot(dados, aes(x = ldose, y = Resposta/n, color=Trat)) +
  geom_point(size = 4) + theme_bw(base_size = 18) + 
  labs(x = 'Dose(log)', y = 'Proporção de insetos mortos') +
  theme(legend.position="top")
h0
### Conforme discutido em aula, vamos considerar três modelos, conforme descrito na sequência:

### ajuste4 - Modelo de retas coincidentes - Sem considerar o efeito de tratamento. 
### ajuste5 - Modelo de retas paralelas - Considerando o efeito de tratamento apenas 
### no intercepto (modelo aditivo). 
### ajuste6 - Modelo de retas concorrentes - Sem considerar o efeito de tratamento 
### no intercepto e associado à dose (modelo multiplicativo - com efeito de interação).. 

################################################################################
### Modelo de retas coincidentes.

ajuste4 <- glm(cbind(Resposta,n-Resposta) ~ ldose, family = binomial, data = dados)
### Este é o modelo mais simples, em que ajustamos uma única reta (na escala do 
### preditor, curva na escala da resposta) para os três tratamentos.
summary(ajuste4)

### Adicionando o modelo ajustado ao gráfico.
preditos4 <- predict(ajuste4, newdata=data.frame(ldose = grid_ldose), type='response')
new.data4 <- data.frame(grid_ldose, preditos4)

h1 <- h0 + geom_line(data = new.data4, aes(x = grid_ldose, y = preditos4),  
                     size = 1.2, color = 'black')
h1

################################################################################
### Modelo de retas paralelas.

ajuste5 <- glm(cbind(Resposta,n-Resposta) ~ ldose + Trat, family = binomial, data = dados)
### Este é o modelo intermediário (aditivo), em que ajustamos retas paralelas 
### (na escala do preditor) para os três tratamentos.
summary(ajuste5)

### Agora, para predição precisamos de um grid de valores para cada inseticida.
### Vamos usar a função expand.grid para isso.

trat <- levels(dados$Trat)
grid_trat <- expand.grid(grid_ldose, trat)
names(grid_trat) <- c("grid_ldose", "trat")
preditos5 <- predict(ajuste5, newdata = data.frame(ldose = grid_trat[,1], Trat = grid_trat[,2]), 
                     type='response')
new.data5 <- data.frame(grid_trat, preditos5)

h0 + geom_line(data = subset(new.data5, trat == 'BHC'), aes(x = grid_ldose, y = preditos5),  
               size = 1.2, color = 'red') +
  geom_line(data = subset(new.data5, trat == 'DDT'), aes(x = grid_ldose, y = preditos5),  
            size = 1.2, color = 'green') +
  geom_line(data = subset(new.data5, trat == 'DDT+BHC'), aes(x = grid_ldose, y = preditos5),  
            size = 1.2, color = 'blue')
### Adicionando as curvas de mortalidade vs dose para cada inseticida. 

################################################################################
### Modelo de retas concorrentes.

ajuste6 <- glm(cbind(Resposta, n-Resposta) ~ ldose * Trat, family = binomial, data = dados)
### Este é o modelo mais completo (multiplicativo - com interação), em que ajustamos 
### retas concorrentes (na escala do preditor) para os três tratamentos.
summary(ajuste6)

preditos6 <- predict(ajuste6, newdata = data.frame(ldose = grid_trat[,1], Trat = grid_trat[,2]), type='response')
new.data6 <- data.frame(grid_trat, preditos6)

h0 + geom_line(data = subset(new.data6, trat == 'BHC'), aes(x = grid_ldose, y = preditos6),  
               size = 1.2, color = 'red') +
  geom_line(data = subset(new.data6, trat == 'DDT'), aes(x = grid_ldose, y = preditos6),  
            size = 1.2, color = 'green') +
  geom_line(data = subset(new.data6, trat == 'DDT+BHC'), aes(x = grid_ldose, y = preditos6),  
            size = 1.2, color = 'blue')

################################################################################
### Comparação dos ajustes dos três modelos.

### Pelo fato dos três modelos serem encaixados, vamos compará-los usando o TRV.
anova(ajuste4, ajuste5, ajuste6, test = 'Chisq')

### Repare, olhando para as duas primeiras linhas, que os modelos de retas coincidentes 
### e retas paralelas produzem desvios significativamente diferentes.
### Nesse caso, dentre os dois modelos, optamos pelo não restrito (retas paralelas). 
### No entanto, ao observar as duas últimas linhas, verificamos que os
### dois ajustes produzem desvios que não apresentam diferença significativa. 
### Nesse caso, podemos optar pelo modelo restrito (retas paralelas).
### Pela análise dos desvios, baseado nos TRVs, o modelo de retas paralelas 
### (efeito de tratamento no intercepto) é preferível.

AIC(ajuste4,ajuste5,ajuste6)
### Repare que também através dos AICs tem-se indicativo de melhor ajuste para o 
### modelo de retas paralelas.

### Observando os resultados do summary, verifica-se que o modelo de retas paralelas 
### tem deviance 21,282, associado a 14 graus de liberdade. Vamos testar a qualidade do ajuste.

pchisq(21.282, 14, lower.tail = F) 
### Ao nível de significância de 5% não se tem evidência significativa de falta de ajuste.

### Exercício - Para o modelo escolhido:

# 1- Escrever o modelo ajustado na escala do preditor, da chance e da probabilidade de resposta;
# 2- Interpretar as estimativas dos parâmetros do modelo;
# 3- Obter ICs 95% para os parâmetros do modelo;
# 4- Estimar a probabilidade de morte para doses de 3 e 7 unidades para cada tratamento.
# 5- Obter ICs 95% para as probabilidades mencionadas no item anterior;
# 6- Estimar, para cada tratamento, as doses letais 50% e 90%;
# 7- Fazer uma análise de diagnóstico, baseada na avaliação dos resíduos e de medidas de influência.