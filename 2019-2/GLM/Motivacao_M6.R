########################################################################
### Pacotes (possivelmente) utilizados
require(rattle.data)
require(statmod)
require(ISLR)
require(car)
require(hnp)
require(pscl)
require(coefplot)
require(effects)
require(sandwich)
require(lmtest)

########################################################################
### Regressão para dados de contagens. Informações referentes a 500 portadores
### de seguro de automóvel de uma particular seguradora. Foram filtrados
### apenas os segurados cujas apólices têm mais de cinco anos. As variáveis
### são as seguintes:

# idade: idade do segurado (em anos);

# sexo: Masc para masculino e Fem para feminino;

# usop: uso principal do veículo. Cidade para uso urbano e Estrada para 
# uso rodoviário;

# anosest: escolaridade do segurado, em anos de estudo;

# claims: número de sinistros produzidos pelo segurado nos últimos cinco
# anos.

### O objetivo aqui é modelar a frequência de sinistros em função das demais
### covariáveis.

########################################################################
########################################################################
########################################################################
### Importação dos dados e análise descritiva.

dados <- read.csv2('https://docs.ufpr.br/~taconeli/CE22517/sinistros.csv')[,-1]
dados <- read.csv2('sinistros.csv')[,-1]

head(dados, 10) ### Dez primeiras linhas da base.
summary(dados) ### Algumas descritivas dos dados.

### Análise descritiva

x11(width = 12, height = 10)
par(las = 1, mar = c(5,4.5,2,2), cex = 1.4)
plot(table(dados$claims), col = 'blue', lwd = 5, xlab = 'Número de sinistros',
     ylab = 'Frequência', xaxt = 'n')
axis(1, 0:9) 
### Distribuição de frequências para o número de sinistros.

d1 <- with(dados, data.frame(table(sexo, factor(claims, levels = 0:9))))
names(d1) <- c("sexo","claims","Freq")
ggplot(data = d1, aes(x = claims, y = Freq)) + geom_bar(stat = "identity") + 
    facet_wrap(~ sexo) + theme_bw(base_size = 18) + ylab("Frequência") + xlab('Número de sinistros')
x11()
d2 <- with(dados, data.frame(table(usop, factor(claims, levels = 0:9))))
names(d2) <- c("usop","claims","Freq")
ggplot(data = d2, aes(x = claims, y = Freq)) + geom_bar(stat = "identity")+ 
    facet_wrap(~ usop) + theme_bw(base_size = 18) + ylab("Frequência") + xlab('Número de sinistros')

ggplot(data = dados, aes(x = anosest, y = claims)) + geom_jitter() +
    theme_bw(base_size = 18) + ylab("Número de sinistros") + xlab('Anos de estudo') +
    geom_smooth(method = 'loess')

ggplot(data = dados, aes(x = idade, y = claims)) + geom_jitter() +
    theme_bw(base_size = 18) + ylab("Número de sinistros") + xlab('Idade') +
    geom_smooth(method = 'loess')

########################################################################
########################################################################
########################################################################
### Ajuste de um modelo linear com erros normais

### Vamos ajustar um modelo de regressão linear, com erros normais, para o número 
### de sinistros.
x11(width = 10, height = 10)
ajuste1 <- lm(claims ~ idade + sexo + usop + anosest, data=dados)
par(mfrow=c(2,2), cex = 1.4, las = 1, mar = c(4,4,2,2))
plot(ajuste1)
### Os resíduos são claramente heterocedásticos (variância aumenta conforme
### a média) e tem distribuição assimétrica. O modelo linear não se ajusta
### bem aos dados.

########################################################################
########################################################################
########################################################################
### Ajuste de um modelo log-linear com resposta poisson

ajuste2 <- glm(claims ~ idade + sexo + usop + anosest, family = poisson(link = 'log'),
               data = dados)
coefficients(ajuste2) 
### Estimativas dos parâmetros de regressão.

### Vamos extrair mais alguns elementos produzidos pela função glm:

head(model.matrix(ajuste2)) 
### Matriz do modelo.

fitted(ajuste2) 
### Valores ajustados pelo modelo, para os 500 indivíduos da base, na escala 
### da resposta.

predict(ajuste2) 
### Valores ajustados pelo modelo, para os 500 indivíduos da base, na escala 
### do preditor.
 
### Recomendo acessar a documentação da função predict.glm para conferir 
### as opções. Voltaremos a ela adiante.

ajuste2$iter 
### Foram necessárias cinco iterações até a convergência no processo de estimação.

ajuste2$weights 
### Esses são os pesos calculados na última iteração do algoritmo de estimação.

summary(ajuste2) 
### Um resumo mais detalhado do modelo ajustado.

vcov(ajuste2)
### matriz de variâncias e covariâncias estimada para os parâmetros de 
### regressão.

### Para fins disáticos, vamos estimar o parâmetro de dispersão. Primeiro
### usando a estatística X^2 de Pearson.
mu <- fitted(ajuste2)
y <- dados$claims
df <- ajuste2$df.residual
phi <- sum(((y-mu)^2)/mu)/df
phi

### Agora usando a deviance. 
dev <- ajuste2$deviance
phi <- dev/df
phi

########################################################################
########################################################################
########################################################################
### Inferências e qualidade do ajuste.

########################################################################
### Teste da razão de verossimilhanças.

### Vamos testar algumas hipóteses usando o TRV. Vamos denotar por B_...
### os parâmetros (betas) do modelo. 

### Hipótese 1- H0: BsexoMasc = 0.

ajH0 <- glm(claims ~ idade + usop + anosest, family = poisson(link = 'log'),
            data = dados)

### ou, de forma abreviada:

ajH0 <- update(ajuste2, claims ~.-sexo)

### Na mão:

### Vamos extrair as log-verossimilhanças maximizadas sob os dois modelos:

l0 <- as.numeric(logLik(ajH0)); l0
l1 <- as.numeric(logLik(ajuste2)); l1

### A estatística do teste da razão de verossimilhanças fica dada por:

Lambda <- -2*(l0-l1); Lambda

### Como estamos testando um único parâmetro, a distribuição de referência
### para o teste é a qui-quadrado com um grau de liberdade. Vamos obter o 
### valor crítico para um nível de significância de 5%:

qchisq(0.05, df = 1, lower.tail = FALSE)

### Como o valor da estatística do teste não excede o valor crítico, a
### hipótese nula (não efeito de sexo) não é rejeitada ao nível de significância
### de 5%. Vamos calcular o p-valor do teste.

pchisq(Lambda, df = 1, lower.tail = FALSE)

### Usando a função anova:

anova(ajH0, ajuste2, test = 'Chisq')

### Exercício: Aplicar o teste da razão de verossimilhanças no teste da
### hipótese nula H0: B_sexoMasc = B_anosest = 0.


### Teste de Wald.

### O resultado do teste de Wald, para cada parâmetro, já é apresentado 
### no summary do modelo. Vamos aplicar o método de Wald para testar a 
### hipótese H0: B_sexoMasc = B_anosest = 0.

### Na mão. Começamos extraindo estimativas pontuais e a parte da matriz
### de variâncias e covariâncias referentes aos dois parâmetros sob teste.

B0 <- matrix(coef(ajuste2)[c(3,5)]); B0
VarB0 <- vcov(ajuste2)[c(3,5),c(3,5)]; VarB0

### Agora, vamos calcular a estatística do teste de Wald:

W <- t(B0) %*% solve(VarB0) %*% B0
W

### Como estamos testando dois parâmetro, a distribuição de referência
### para o teste é a qui-quadrado com dois graus de liberdade. Vamos obter o 
### valor crítico para um nível de significância de 5%:

qchisq(0.05, df = 2, lower.tail = FALSE)

### Como o valor da estatística do teste não excede o valor crítico, a
### hipótese nula (não efeito de sexo e escolaridade) não é rejeitada ao 
### nível de significância de 5%. Vamos calcular o p-valor do teste.

pchisq(W, df = 2, lower.tail = FALSE)

### Agora, vamos usar a função waldtest do pacote lmtest:

ajH0 <- update(ajuste2, claims ~.-sexo - anosest)
waldtest(ajH0, ajuste2, test = 'Chisq')

### Exercício: Aplicar o método de Wald no teste da hipótese nula 
### H0: B_sexoMasc = B_idade = 0.

########################################################################
### Intervalos de confiança

### Agora, intervalos de confiança. Começando pelo método de Wald. Vamos
### obter um intervalo de 95% de confiança para B_idade:

Bchap <- coef(ajuste2)[2]; Bchap
VBchap <- vcov(ajuste2)[2,2]; VBchap
ICB_idade <- c(Bchap - 1.96 * sqrt(VBchap), Bchap + 1.96 * sqrt(VBchap))
ICB_idade

### Como alternativa, podemos usar a função confint.default:
confint.default(ajuste2, parm = "idade", level = 0.95)
confint.default(ajuste2, parm = "idade", level = 0.99)
confint.default(ajuste2, level = 0.95)

### Se quisermos um IC(95%) para exp(B_idade), basta exponenciar os limites
### do IC obtido para B_idade:

exp(ICB_idade)
### Procedimento semelhante valeria para qualquer outra função monótona
### de B_idade.

### Vamos ilustrar agora a construção do IC baseado no perfil da verossimilhança.
### Para isso, precisamos obter o valor da (log) verossimilhança maximizada
### para um grid de valores de 

B_idade_grid <- seq(-0.08, -0.035, length.out = 20)
### Agora, para cada valor de B_idade em B_idade_grid vamos ajustar novamente
### o glm, mas fixando B_idade. Para cada ajuste vamos extrair o valor da 
### log-verossimilhança maximizada. Antes, vamos aplicar esse procedimento
### a um particular valor de B_idade, para fins ilustrativos. Tomemos
### B_idade=-0.05.

ajuste <- glm(claims ~ offset(-0.05 * idade) + usop + anosest + sexo, family = poisson(link = 'log'),
              data = dados)
logLik(ajuste)
### log-verossimilhança maximizada para B_idade=-0.05. Agora, vamos aplicar
### igual procedimento para o grid de valores de B_idade.

vet_logLik <- numeric()
for(i in 1:length(B_idade_grid)){
    B_idade <- B_idade_grid[i]
    ajuste <- glm(claims ~ offset(B_idade * idade) + usop + anosest + sexo, family = poisson(link = 'log'),
    data = dados)
    vet_logLik[i] <- logLik(ajuste)
}

### O argumento offset() acrescenta o termo correspondente ao preditor 
### do modelo sem um parâmetro a ser estimado. É a forma usada para fixar
### o valor de beta em cada componente de B_idade_grid.

### Agora, vamos calcular a estatística do teste da razão de verossimilhanças
### para cada valor de B_idade_grid.

par(cex = 1.4, las = 1)
plot(B_idade_grid, vet_logLik, type = 'b', pch = 20, xlav = 'B_idade', ylab = 'Log-verossimilhança')
### Gráfico do perfil da log-verossimilhança.

### O intervalo de confiança 95% baseado no perfil da verossimilhança irá conter
### todos os valores de B_idade0 tais que -2*[l(B_idade0) - l(B_idade_chap)] < 3.84,
### em que l(B_idade0) é a log-verossimilhança maximizada fixando  
### B_idade em B_idade0, l(B_idade_chap) é a log-verossimilhança maximizada
### não fixando qualquer particular valor para B_idade e 3.84 é o quantil
### 0.95 da distribuição qui-quadrado com um grau de liberdade.

abline(h = logLik(ajuste2) - 3.84/2)

### Podemos calcular o IC baseado no perfil da verossimilhança usando a 
### função confint:
confint(ajuste2, parm = "idade", level = 0.95)
confint(ajuste2, parm = "idade", level = 0.99)
confint(ajuste2)

### Agora, para fins ilustrativos, vamos plotar os limites de confiança
### sobre o gráfico.
abline(v = c(-0.06855816, -0.04885103), lty = 2, col = 'red')

### Exercício: Construir o gráfico do perfil da verossimilhança e obter
### o IC(95%) para B_sexoM.

### Gráficos de efeitos
plot(allEffects(ajuste2)) ### Escala do link.
plot(allEffects(ajuste2), type = 'response') ### Escala da resposta.

########################################################################
########################################################################
########################################################################
### Predições

### Agora, vamos estimar o número médio de sinistros para alguns perfis
### de segurados. Primeiro, para segurados de 30 anos que fazem uso principal 
### na estrada, sexo masculino e 10 anos de estudo.

x0 <- matrix(c(1, 30, 1, 1, 10))
x0 ### Vetor de covariáveis. o primeiro termo corresponde ao intercepto.

betas <- matrix(coef(ajuste2))
betas
    
t(x0) %*% betas ### Estimativa na escala do preditor (log(mu)).

exp(t(x0) %*% betas) ### Estimativa na escala da média. Assim, para o perfil
### de segurado considerado, estima-se, em média, 3.11 sinistros.
    
### Agora, vamos usar a função predict.
predict(ajuste2, newdata = data.frame(idade = 30, usop = 'Estrada', 
                                      sexo = 'Masc', anosest = 10))
### Por default, o R retorna a predição na escala do link. Para obter a
### estimativa da média devemos aplicar a inversa da função de ligação:

predict(ajuste2, newdata = data.frame(idade = 30, usop = 'Estrada', sexo = 'Masc', 
                                      anosest = 10), type = 'response')
### Agora, na escala da média, ou da resposta.

### Podemos realizar predições para toda uma base de novos indivíduos. Uma
### pequena ilustração:

dpred <- data.frame(idade = c(30,30,60,60),
                    usop = c('Estrada','Cidade','Estrada','Cidade'),
                    sexo = c('Masc', 'Fem', 'Fem', 'Masc'),
                    anosest = c(5, 7, 12, 15))

mu_est <- predict(ajuste2, newdata = dpred, type = 'response')
dpred$mu <- mu_est
rownames(dpred) <- c('Perfil 1', 'Perfil 2', 'Perfil 3', 'Perfil 4')
dpred
### Estimativas para o número esperado de sinistros em cinco anos para os
### quatro perfis de segurados.

### Vamos ver a distribuição de probabilidades estimada para cada perfil.
### Primeiro, vamos calcular as probabilidades ajustadas, usando as médias
### estimadas para cada perfil.

Perfil1 <- dpois(0:10, mu_est[1])
Perfil2 <- dpois(0:10, mu_est[2])
Perfil3 <- dpois(0:10, mu_est[3])
Perfil4 <- dpois(0:10, mu_est[4])

### Agora vamos plotar as probabilidades para avaliar as distribuições.
x11(width = 12, height = 10)
par(mfrow = c(2,2), cex = 1.3, las = 1, mar = c(5,4,2,2), las = 1)
plot(0:10, Perfil1, type = 'h', lwd = 3, xlab = 'x', main = 'Perfil 1', 
     xaxt = 'n', ylim = c(0, 0.7))
axis(1, 0:10)
plot(0:10, Perfil2, type = 'h', lwd = 3, xlab = 'x', main = 'Perfil 2', 
     xaxt = 'n', ylim = c(0, 0.7))
axis(1, 0:10)
plot(0:10, Perfil3, type = 'h', lwd = 3, xlab = 'x', main = 'Perfil 3', 
     xaxt = 'n', ylim = c(0, 0.7))
axis(1, 0:10)
plot(0:10, Perfil4, type = 'h', lwd = 3, xlab = 'x', main = 'Perfil 4', 
     xaxt = 'n', ylim = c(0, 0.7))
axis(1, 0:10)

### Intervalos de confiança para predições. Vamos considerar, novamente, 
### de 30 anos que fazem uso principal na estrada, sexo masculino e 10 anos de estudo.

### Primeiro na unha.

x0 <- matrix(c(1, 30, 1, 1, 10)) ### Vetor de covariáveis.
### Intercepto, idade, indicadora de uso na estrada e produto (idade*uso).  

betaChap <- coef(ajuste2)
### Vetor de estimativas dos parâmetros de regressão.

etaChap <- crossprod(x0, betaChap)
### Predição na escala do preditor.

varChap <- vcov(ajuste2)
### Matriz de variâncias e covariâncias estimada para os estimadores dos
### parâmetros de regressão.

VarEtaChap <- t(x0) %*% varChap %*% x0 
### Erro padrão de etaChap.

c(etaChap -1.96 * sqrt(VarEtaChap), etaChap +1.96 * sqrt(VarEtaChap))
### IC 95% (na escala do preditor)

exp(c(etaChap -1.96 * sqrt(VarEtaChap), etaChap +1.96 * sqrt(VarEtaChap))) 
### IC 95% (na escala da resposta)

### Agora, usando a função predict.

pred_link <- predict(ajuste2, newdata = data.frame(idade = 30, usop = 'Estrada', sexo = 'Masc', 
                                                       anosest = 10), se.fit = TRUE)
### Declarando se.fit = TRUE, a função retorna também o erro padrão de 
### predição.
pred_link

pred_link$fit + c(-1.96, 1.96) * pred_link$se.fit
### IC 95% (na escala do preditor)

exp(pred_link$fit + c(-1.96, 1.96) * pred_link$se.fit)
### IC 95% (na escala da resposta)

### Agora, vamos obter intervalos de confiança para a frequência média de
### sinistros no atacado, considerando todos os vetores de covariáveis
### correspondentes aos 500 indivíduos da base.

pred_base <- predict(ajuste2, type = 'link', se.fit = TRUE)
ic <- cbind(pred_base$fit - 1.96 * pred_base$se.fit, pred_base$fit + 1.96 * pred_base$se.fit)
data.frame(dados[,1:4], exp(pred_base$fit), exp(ic))
### Base de dados acompanhada das estimativas para a frequência média de
### sinistros e IC(95%).

########################################################################
########################################################################
########################################################################
### Análise de deviance

### Modelo nulo - modelo só com intercepto (um único parâmetro, comum a
### todas as observações)
ajusteNull <- glm(claims ~ 1, family = poisson(link = 'log'), data = dados)
summary(ajusteNull)
logLik(ajusteNull) ### log verossimilhança maximizada.
deviance(ajusteNull) ### deviance.

### Modelo saturado - modelo com n parâmetros (um para cada observação)
obs <- factor(1:nrow(dados))
ajusteSat <- glm(claims ~ obs, family=poisson(link='log'),data=dados)
logLik(ajusteSat) ### log verossimilhança maximizada.
deviance(ajusteSat) ### deviance.

### Para o modelo ajustado com as quatro covariáveis:
logLik(ajuste2) 
deviance(ajuste2)

### Análise de deviance

anova(ajuste2, test='Chisq')
### Repare na sequência de testes que são realizados:
### 1- Inclusão de idade ao modelo nulo (altamente significativa);
### 2- Inclusão de sexo ao modelo com idade (não significativa);
### 3- Inclusão de uso principal ao modelo com idade e sexo (altamente significativa);
### 4- Inclusão de anos de estudo ao modelo com as demais variáveis (não significativa).

Anova(ajuste2, test = 'LR')
### Repare na sequência de testes que são realizados:
### 1- Inclusão de idade ao modelo com as demais três variáveis (altamente significativa);
### 2- Inclusão de sexo ao modelo com as demais três variáveis (não significativa);
### 3- Inclusão de uso principal ao modelo com as demais três variáveis (altamente significativa);
### 4- Inclusão de anos de estudo ao modelo com as demais três variáveis (não significativa).

### Experimente entrar na função glm com as variáveis em outra ordem e 
### usar anova e Anova. Observe o que muda.

drop1(ajuste2,test = 'Chisq') 
### Elimina uma variável por vez do modelo e testa a exclusão da variável 
### via TRV. (similar a Anova)
### Não se verifica alteração significativa no ajuste resultante da exclusão 
### do sexo, nem da exclusão dos anos de estudo. 
### Para as outras duas, há fortes evidências que idade e uso principal 
### estejam relacionadas ao número de sinistros.

### Vamos explorar um pouco mais a parte de testes de hipóteses. Novamente,
### vamos testar o efeito de escolaridade e sexo, conjuntamente.

ajuste2_rest <- update(ajuste2, ~.-sexo-anosest)
### Modelo sob a restrição.

anova(ajuste2_rest, ajuste2, test = 'Chisq')
### Não há efeito significativo de escolaridade e sexo. Podemos removê-las
### do modelo ajustado. Adicionalmente, vamos avaliar se há efeito de 
### interação entre as variáveis remanescentes (uso principal e idade).

ajuste2 <- update(ajuste2_rest, ~.+idade:usop)
anova(ajuste2_rest, ajuste2, test = 'Chisq')
### O efeito de interação é significativo. Fica como exercício explorar 
### esse ajuste (escrever a expressão do modelo ajustado, apresentar 
### algumas inferências e interpretações, fazer predições). Apenas adiantando,
### segue o resumo do ajuste.

summary(ajuste2)

########################################################################
########################################################################
########################################################################
### Análise de deviance - avaliação da qualidade do ajuste.

### Vamos usar este modelo para avaliar a qualidade do ajuste com base na 
### deviance residual. Vamos usar, a princípio, a aproximação com a distribuição
### qui-quadrado com n-p (496) graus de liberdade.

ajuste2$deviance 
### Deviance residual (estatística do teste da qualidade do ajuste).

qchisq(0.05, df = ajuste2$df.residual, lower.tail = FALSE)
### Valor crítico.

### Como a deviance excede o valor crítico, rejeitaríamos ao nível de 
### significância de 5%, a hipótese nula de que o modelo ajustado está
### corretamente especificado. Vamos ver o valor-p do teste:

pchisq(ajuste2$deviance, df = ajuste2$df.residual, lower.tail = FALSE)
### p = 0.018, reforçando a evidência de que o modelo não é adequado.
### No entanto, é bem conhecido que a aproximação da deviance pela distribuição
### qui-quadrado (n-p), sob a hipótese nula de que o modelo é correto, nem
### sempre é apropriada. Então, como alternativa, vamos obter a distribuição
### sob H0 via simulação.

### Para isso, precisamos simular dados sob a hipótese nula, de que o modelo
### é correto. Como fazemos isso?

### Considere a primeira linha da base:
dados[1,]

### Para esse indivíduo, o valor ajustado pelo modelo é dado por:
fitted(ajuste2)[1]

### Podemos simular uma resposta para essa observação, a partir do modelo,
### da seguinte forma:

rpois(1, fitted(ajuste2)[1])
### pronto. este valor foi simulado do modelo. Da mesma forma podemos simular
### uma observação para cada linha da base:

dados$y <- rpois(500, fitted(ajuste2))
dados$y
### Agora, usamos esse vetor de respostas em substituição ao vetor original
### no ajuste do modelo.

ajuste2Sim <- glm(y~ idade + anosest + usop + sexo, family = poisson, data = dados)
deviance(ajuste2Sim)
### Esta é uma deviance simulada sob H0. Para construir a distribuição
### empírica sob H0, temos que repetir a simulação um grande número de
### vezes (digamos 1000).

vet_deviance <- numeric() ### Vetor que vai armazenar as deviances.

for(i in 1:1000){
    dados$y <- rpois(500, fitted(ajuste2))
    ajuste2Sim <- glm(y~ idade + anosest + usop + sexo, family = poisson, data = dados)
    vet_deviance[i] <- deviance(ajuste2Sim)
}

hist(vet_deviance)
abline(v = ajuste2$deviance, col = 'red')

### Fica bem evidente que o valor calculado da deviance não é atípico ou 
### extremo em relação aos valores simulados sob H0, o que é um indicativo
### de que o modelo se ajusta bem aos dados. Vamos calcular o p-valor 
### com base na distribuição empírica, pela proporção de valores simulados
### que excedem o valor original da deviance.

mean(vet_deviance > deviance(ajuste2))
### Ou seja, não temos evidência de falta de ajuste do modelo.

########################################################################
########################################################################
########################################################################
### Diagnóstico do ajuste.

x11(width = 10, height = 10)
par(mfrow=c(2,2), cex = 1.25, las = 1, mar = c(4,4,2,2))
plot(ajuste2)

### Os resíduos utlizados ao plotar um objeto da classe glm são os resíduos
### componentes da deviance. Nem sempre esses resíduos têm boa aproximação
### com a distribuição normal se o modelo ajustado estiver correto. Isso
### ocorre, particularmente, para dados binários ou de contagens. Esse
### fato será verificado através de um novo estudo por simulação, apresentado
### ao final dos scripts. Voltando à análise dos dados,
### é possível notar comportamento mais aceitável (variância mais homogênea,
### melhor aproximação com a distribuição Normal) em relação ao que foi 
### verificado para o modelo linear.

### para uma melhor apreciação do comportamento dos resíduos, vamos proceder
### com a análise dos resíduos quantílicos aleatorizados e a obtenção do
### gráfico meio normal com envelopes simulados.

residuos <- qres.pois(ajuste2)
ajustados <- predict(ajuste2)

x11(width = 12, height = 8)
par(las = 1, mar = c(4,4,2,2), mfrow = c(1,2), cex = 1.4)
plot(residuos ~ ajustados, col = 'blue', xlab = 'Valores ajustados', ylab = 'Resíduos')
lines(lowess(residuos ~ ajustados), col = 'red', lwd = 2)
qqnorm(residuos, col = 'blue', main = '')
qqline(residuos, lty = 2)
### A curvatura da função suave, presente no gráfico à esquerda, é resultante
### da pequena quantidade de pontos nos extremos dos valores ajustados.
### De qualquer forma, fica evidente que os resíduos tem dispersão aproxim.
### constante e boa aproximação com a distribuição Normal.

shapiro.test(residuos) ### teste de Shapiro-Wilks para normalidade.
### A hipótese nula de normalidade não é rejeitada.

### Como alternativa, vamos usar os gráficos meio normais com envelopes 
### simulados.
x11()
par(las = 1, mar = c(5,4.5,2,2), cex = 1.4)
hnp(ajuste2)
### O gráfico indica que o modelo se ajusta bem aos dados.

### Gráficos de resíduos parciais
### Usando recursos de diagnóstico do pacote car:
x11(width = 15, height = 15)
crPlots(ajuste2, cex = 2, pch = 20) 
### Gráficos de resíduos parciais

residualPlots(ajuste2, cex = 2, pch = 20) 
### Gráfico de resíduos versus variáveis ajustadas no modelo.

########################################################################
########################################################################
########################################################################
### Diagnóstico de outliers e pontos influentes.

### Vamos fazer uma análise de influência usando recursos do pacote car.
influenceIndexPlot(ajuste2, vars = c('Studentized','Cook','Hat'), id.n = 3)
### O gráfico dos resíduos studentizados não indica observações com resíduos extremos.
### O gráfico da distância de Cook tem dois pontos mais discrepantes: 15 e 385.
### O gráfico dos valores de h aponta a observação 295 como possível ponto de alavanca.

### Vamos avaliar os dados destacados na análise.
dados[15,]
dados[295,]
dados[385,]

### Vamos ver o summary dos dados e do ajuste para entender melhor o por 
### que desse dados serem destacados.
summary(dados)
summary(ajuste2)

# O indivíduo 15 gerou nove sinistros, sendo o segurado com maior número 
# de sinistros;

# O indivíduo 385 tem 47 anos e usa o carro na cidade, mas registrou um 
# número elevado de sinistros (cinco).

# O indivíduo 295 é atípico, tem 16 anos de idade e 16 de estudo!!! Deve 
# ser erro de registro.

### Vamos tirar da base as observações 15, 295 e 385 e reajustar o modelo.

ajuste3 <- update(ajuste2, subset= -c(15,295,385))
compareCoefs(ajuste2, ajuste3) 
multiplot(ajuste2, ajuste3) + theme_bw(base_size = 20)
### Comparação de estimativas e erros padrões produzidos pelos dois modelos.
### Os resultados (e, consequentemente, as inferências) não mudam de forma
### substancial ao remover as três informações da base.



