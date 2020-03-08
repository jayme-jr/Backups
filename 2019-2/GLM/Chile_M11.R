require(car)
require(MASS)
require(faraway)
require(effects)
help(Chile)

options(scipen = 3, device = 'x11')
### Preparando os dados.

head(Chile)
summary(Chile)

Chile2 <- Chile[which(Chile$vote%in%c('N','Y')),] 
### Excluindo indecisos e eleitores que pretendem se abster. 

Chile2$vote <- factor(Chile2$vote) 
### Eliminando níveis de 'vote' que não aparecem na nova amostra.

summary(Chile2)
Chile2$education <- factor(Chile2$education,levels=c('P','S','PS'))

### Uma breve análise descritiva
round(prop.table(table(Chile2$vote,Chile2$region),2),2) 
### Tabela de frequências de intenção de voto segundo a região do eleitor.

barplot(table(Chile2$vote,Chile2$region), beside=T, ylim=c(0,350), las = 1,
        xlab = 'Região', ylab = 'Nº de Eleitores', legend = c('Não','Sim')) 
### Gráfico de barras - intenção de voto vs região. 

barplot(table(Chile2$vote, Chile2$sex), beside = T, las = 1, xlab = 'Sexo',
        ylab = 'Nº de Eleitores', legend = c('Não','Sim'))
### Gráfico de barras - intenção de voto vs sexo 

barplot(table(Chile2$vote, Chile2$education), beside = T, las = 1, xlab = 'Escolaridade',
        ylab = 'Nº de Eleitores', legend = c('Não','Sim'))
### Gráfico de barras - intenção de voto vs Educação 

boxplot(Chile2$age ~ Chile2$vote, xlab = 'Voto', ylab = 'Idade de eleitores',
        names = c('Não','Sim')) 
### Boxplot de idade versus intenção de voto.

boxplot(Chile2$income ~ Chile2$vote, ylab = 'Renda')
### Boxplot de renda versus intenção de voto.

boxplot(Chile2$statusquo ~ Chile2$vote, ylab = 'Status quo')
### Boxplot de statusquo versus intenção de voto.

### Vamos ajustar um glm para a intenção de voto, considerando distribuição 
### binomial com função de ligação logito.

Chile3 <- na.omit(Chile2) ### Eliminando da base as linhas com NA's.
intvoto <- glm(vote ~ ., family = binomial(link = 'logit'), data = Chile3)
summary(intvoto) ### Resumo do modelo ajustado. 
### Nota-se menor intenção de votos favoráveis ao regime dentre os homens, 
### dentre os eleitores com maior escolaridade e maior intenção quanto maior o status-quo.

### É evidente (e até mesmo previsível) que a variável statusquo seja extremamente 
### relacionada à intenção de voto (no caso, ao voto favorável), 
### sendo os efeitos das demais covariáveis bastante modestos
### se comparados a ela. O que aconteceria se tirássemos ela do modelo?

intvoto2 <- update(intvoto, ~. -statusquo)
summary(intvoto2) ### Observe que alguns efeitos que anteriormente não 
### apresentavam significância estatística agora apresentam.
### Vamos optar pelo modelo sem a inclusão de statusquo.

Anova(intvoto2, test = 'LR') 
### Os testes das razões de verossimilhanças indicam a significância estatística 
### das variáveis incluídas no modelo.

anova(intvoto2, test = 'LR') 
### Qual a diferença com relação ao comando acima mesmo?

### Vamos avaliar possível efeito de interação entre sexo e educação:
intvoto4 <- update(intvoto2, ~ . +sex:education)
anova(intvoto2, intvoto4, test='LR')

### Repare no efeito não significativo da interação. Deixo como exercício verificar 
### o efeito de alguma interação de interesse e, caso verificada a significância,
### incluí-la no modelo e analisar os resultados.

### Exercício 1 - escrever a equação do modelo ajustado. Interpretar cada um dos 
### coeficientes estimados.

### Vamos calcular a probabilidade de intenção de voto pró-Pinochet para eleitores 
### com os seguintes perfis:

# Região C, População 25.000, Sexo masculino, idade 35 anos, com ensino superior, renda 120.000.
# Região M, População 50.000, Sexo feminino, idade 55 anos, com ensino primário, renda 15.000.

datapred <- data.frame(region = c('C','M'), population = c(25000,50000),
                       sex = c('M','F'), age = c(35,55), education = c('S','P'),
                       income = c(120000,15000))
### Criando um data frame para os dados que vamos predizer.

predict(intvoto2, newdata=datapred, type='response') 
### estimativas pontuais para as probabilidades de voto para os dois perfis.

### Agora, vamos obter ICs 95% para as probabilidades correspondentes aos dois perfis de eleitores. 
p1 <- predict(intvoto2,newdata=datapred,type='link',se.fit=T); p1 
### Estimativas na escla do preditor, pedindo ao R os erros padrões.

### Agora, vamos calcular os intervalos de confiança para as probabilidades estimadas.
# Para o perfil 1:
ic1 <- p1$fit[1]+c(-1.96,1.96)*p1$se.fit[1] ### Intervalo de confiança 95% para o preditor.
ilogit(ic1) ### Convertendo os limites para a escala de pi. 
### A função ilogit(x) calcula (exp(x)/(exp(x)+1)

# Para o perfil 2:
ic2 <- p1$fit[2]+c(-1.96,1.96)*p1$se.fit[2] ### Intervalo de confiança 95% para o preditor.
ilogit(ic2) 
### Convertendo os limites para a escala de pi.

# O pacote effects dispõe de gráficos que permitem visualizar o efeito das covariáveis na resposta.

plot(effect("income", intvoto2), type = 'response') 
### Nesse gráfico, temos as probabilidades estimadas de votos pró-Pinochet 
### (com ICs 95%) segundo a renda. Os valores das demais covariáveis são fixados
### na média (consultar a documentação da função para maiores detalhes).

plot(effect("income", intvoto2), type = 'link') 
### Gráfico do efeito de renda na escala do preditor.

plot(effect("age", intvoto2), type = 'response')
### Gráfico para o efeito de idade.

### Um pouco de diagnóstico.

par(mfrow=c(2,2))
plot(intvoto2, 1:4) ### Gráficos de diagnóstico padrão para a função glm 
### (baseados nos resíduos componentes da deviance). Complicado avaliar.

require(car)
influenceIndexPlot(intvoto4, vars = c('Studentized','Cook','Hat'), id.n = 3, cex = 1.4)
### Não há indicativos fortes de out-liers ou observações influentes. 
### Apenas como exercício, vamos verificar o indivíduo com identificação 2669,
### que produziu maior valor para a distância de Cook.

Chile3['2669',]
### Repare que é uma pessoa com características pró-Pinochet (mulher, 67 anos...), 
### mas que vota não. Como a base é bastante grande, dificilmente essa observação
### tenha, sozinha, muita influência no ajuste. Mas, a título de esercício, 
### vamos ajustar um novo modelo sem ela e comparar os resultados.

intvoto22 <- update(intvoto2, data = Chile3[-which(rownames(Chile3) == '2669'),])
compareCoefs(intvoto2, intvoto22) ### Sem grandes mudanças.

### Agora, vamos avaliar a qualidade do ajuste com base nos resíduos quantílicos 
### aleatorizados e no gráfico do resíduo da deviance com envelope simulado.

## Usando os resíduos quantílicos:
require(statmod)
residuos<-qresiduals(intvoto2)
qqnorm(residuos)
qqline(residuos)
### Os resíduos apresentam boa aderência à distribuição Normal, indicativo
### de bom ajuste.

## Agora o gráfico de resíduos com envelopes simulados.
require(hnp)
hnp(intvoto2) ### Resíduos dispersos no interior dos envelopes simulados,
### sem qualquer padrão sistemático. Modelo bem ajustado.

### Para finalizar, vamos testar a qualidade do ajuste com base na estatística C 
### de Hosmer e Lemeshow.

### Função para o cálculo da Estatística (e teste) de qualidade de ajuste 
### proposta por Hosmer e Lemeshow.
### Você deve entrar com o vetor de predições (na escala da resposta), 
### o vetor de valores observados (zeros e uns) e o número de grupos (g) a serem formados.

CHosmer <- function(modelo,g){
respostas <- modelo$y
preditos <- predict(modelo, type = 'response')

dpred <- data.frame(preditos, respostas) 
### Dataframe com probabilidades estimadas e respostas para cada indivíduo.

dpred <- dpred[order(dpred[,1]),] 
### Ordenando as linhas do dataframe da menor para a maior probabilidade estimada.

cortes <- quantile(dpred[,1], probs=seq(0,1, 1/g), include.lowest=TRUE) 
### Calculando os quantis para as probabilidades estimadas, para posterior formação dos grupos.

c1 <- cut(dpred[,1],breaks=cortes,include.lowest=T) 
### Formando g grupos, de tamanhos (aproximadamente) iguais, com probabilidades 
### estimadas semelhantes.

Obs <- tapply(dpred[,2], c1, sum) 
### Obs é um vetor com o número observado de respostas em cada um dos g grupos.

pi <- tapply(dpred[,1], c1, mean) 
### pi é um vetor com as médias das probabilidades estimadas em cada um dos g grupos.

n <- tapply(dpred[,1], c1, length) 
### n é um vetor com os tamanhos de amostras em cada grupo.

Cchap <- sum(((Obs-n*pi)**2)/(n*pi*(1-pi))) 
### Estatística do teste de qualidade proposto por Hosmer e Lemeshow e

pv <- 1 - pchisq(Cchap, g-2) ### p-valor correspondente.
return(list(Cchap=Cchap, pvalue=pv))
}

CHosmer(intvoto2,g=10)
### C=4,37 que, com p-valor=0,822. Assim, não se tem evidência significativa de falta de ajuste.