library(tidyverse)
library(kableExtra)
library(readxl)
library(gridExtra)
library(corrplot)
library(MASS)
library(hnp)
library(car)
library(statmod)
library(effects)

## Bases
munic <- read.csv2("municipios", sep = ";", header = TRUE) %>% 
  mutate(`Município` = str_sub(`Município`, start = 8)) 

munic <- munic[-c(74, dim(munic)[1]),]


munic <- munic %>% dplyr::select(`Município`)

MR1 <- read.csv2("base_MR1", sep = ";", header = TRUE) %>% 
  mutate(`Município` = str_sub(`Município`, start = 8),
         `Macrorregião` = 1) 

MR2 <- read.csv2("base_MR2", sep = ";", header = TRUE) %>% 
  mutate(`Município` = str_sub(`Município`, start = 8),
         `Macrorregião` = 2)

MR3 <- read.csv2("base_MR3", sep = ";", header = TRUE) %>% 
  mutate(`Município` = str_sub(`Município`, start = 8),
         `Macrorregião` = 3)

MR3$`Óbitos.p.Ocorrênc`[22] <- MR3$`Óbitos.p.Ocorrênc`[22] + MR3$`Óbitos.p.Ocorrênc`[13]
MR3 <- MR3[-13,]

MR4 <- read.csv2("base_MR4", sep = ";", header = TRUE) %>% 
  mutate(`Município` = str_sub(`Município`, start = 8),
         `Macrorregião` = 4)

veic <- read_xlsx("base_veic.xlsx", sheet = "TODOS") %>% 
  dplyr::select(`Município`, n_veiculos)
    

veic$`Município`[102] <- "Eldorado do Carajás"


pop <- read_xlsx("base_pop.xlsx") 
pop$Espacialidades[47] <- "Eldorado do Carajás"
pop$Espacialidades[108] <- "Santa Izabel do Pará"

pop <- pop %>% 
  rename("Município" = Espacialidades, "pop_urb" = `População urbana 2010`, 
         "pop_total" = `População total 2010`, "per_capta" = `Renda per capita 2010`) %>% 
  mutate(pop_urb = round(pop_urb/pop_total, digits = 2))
  

mr1 <- c("Afuá", "Anajás", "Bagre", "Chaves", "Curralinho", "Gurupá", "Melgaço", "Santa Cruz do Arari", "Porto de Moz")
mr2 <- c("Colares", "Quatipuru", "Maracanã")
mr3 <- c("Faro")
mr4 <- c("Bannach", "Brejo Grande do Araguaia", "Palestina do Pará")

MR <- MR1 %>% 
  bind_rows(MR2) %>% 
  bind_rows(MR3) %>% 
  bind_rows(MR4)

base <- munic %>% 
  left_join(MR) %>% 
  left_join(veic) %>% 
  left_join(pop) %>% 
  rename("Fatalidades" = `Óbitos.p.Ocorrênc`) %>% 
  mutate(Fatalidades = ifelse(is.na(Fatalidades), 0, Fatalidades),
         `Macrorregião` = ifelse(`Município` %in% mr1, 1, 
                                ifelse(`Município` %in% mr2, 2,
                                       ifelse(`Município` %in% mr3, 3, 
                                              ifelse(`Município` %in% mr4, 4, `Macrorregião`)))))#,
         # `Macrorregião` = factor(`Macrorregião`, levels = c(1, 2, 3, 4))) 

## Descritiva
summary(base)

tabela1 <- head(base, n = 10)
T_1 <- kableExtra::kable(tabela1, align = 'lcccccc', booktabs = TRUE) %>% 
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))
## Box-Plots
par(mfrow=c(2,3))

for (i in 2:ncol(base)) {
  boxplot(base[,i], 
          xlab = '', 
          ylab = '', 
          main = names(base[i]), 
          las=1,
          col = '#C8F9F3')
}

g1 <- ggplot(base, aes(x=Fatalidades)) + geom_histogram()+ xlab('Fatalidades')+ ylab('')
g2 <- ggplot(base, aes(x=`Macrorregião`)) + geom_histogram()+ xlab('Macrorregião')+ ylab('')
g3 <- ggplot(base, aes(x=n_veiculos)) + geom_histogram()+ xlab('Frota de Veículos')+ ylab('')
g4 <- ggplot(base, aes(x=pop_total)) + geom_histogram()+ xlab('Popupalação')+ ylab('')
g5 <- ggplot(base, aes(x=pop_urb)) + geom_histogram()+ xlab('Taxa de População Urbana')+ ylab('')
g6 <- ggplot(base, aes(x=per_capta)) + geom_histogram()+ xlab('Renda Per Capta')+ ylab('')

grid.arrange(g1, g2, g3, g4, g5, g6, nrow=2, ncol=3)
## Pelos histogramas ficou evidente que as variáveis frota de carros, população e renda per capta são assimétricas.
## Então pode ser considerada uma transformação para estas variáveis.
## A variável resposta(Fatalidades) também se mostrou assimétrica devido a alguns municípios terem muito mais casos
## que outros.

### Transformando variáveis explicativas
base$log_n_veiculos <- log(base$n_veiculos)
base$log_pop_total  <- log(base$pop_total)
base$log_per_capta <- log(base$per_capta)

## E verificar novamente a forma da distribuição das variáveis transformadas:
  
  par(mfrow = c(1,3))

hist(base$log_n_veiculos, main = 'log(Frota)', xlab = '', ylab = '', col = '#F3EBAB')
hist(base$log_pop_total, main = 'log(População)', xlab = '', ylab = '', col = '#F3EBAB')
hist(base$log_per_capta, main = 'log(Renda Per Capta)', xlab = '', ylab = '', col = '#F3EBAB')


## Correlação
par(mfrow = c(1,1))
cor <- cor(base[ , c(2,3,6,8,9,10)], use = "na.or.complete")

corrplot.mixed(cor, upper = "ellipse")
## Nenhuma das variáveis mostrou forte correlação com a resposta ou entre si.

### Gráfico de disperssão

plot(base[ , c(2,3,6,8,9,10)] , 
     pch=20 , 
     cex=1.5 , 
     col=rgb(0.5, 0.8, 0.9, 0.7))

## Gráfico de correlação mostra que a variável resposta tem valores altos atípicos, distante das nuvens de pontos.

### Modelos Poisso e Binomial Negativa com ligação logarítmica.

m1 <- glm(Fatalidades ~ `Macrorregião` + log_n_veiculos + log_pop_total + pop_urb + log_per_capta, 
          data = base, family = 'poisson')

m2 <- glm.nb(Fatalidades ~ `Macrorregião` + log_n_veiculos + log_pop_total + pop_urb + log_per_capta, 
             data = base)
### Escolha do Modelo

ajuste = c('Poisson', 'Binomial Negativa')

aic    = c(AIC(m1), AIC(m2))

verossimilhança = c(logLik(m1),logLik(m2))

df <- data.frame(ajuste, aic, verossimilhança)

T_2 <- kableExtra::kable(df, align = 'lcc', booktabs = TRUE) %>% 
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))

## Modelo selecionado foi o com menor AIC e maior Verossimilhança, Binomial Negativa.


## Gráficos de envelopes simulados
par(mfrow = c(1,2))

hnp(m1, xlab = 'Percentil da N(0,1)', 
    ylab = 'Resíduos', 
    main = 'Poisson')

hnp(m2, xlab = 'Percentil da N(0,1)', 
    ylab = 'Resíduos', 
    main = 'Binomial Negativa')

## Nota-se pelas medidas de qualidade de ajuste e o comportamento dos resíduos no gráfico a total falta de 
## aderência à distribuição de Poison; alterando a distribuição da resposta para Binomial Negativa obteve-se 
## um ajuste satisfatório.

## Vamos seguir as análises fazendo uso do modelo Binomial Negativo.

### Resumo do modelo

summary(m2)

## Pelo resumo, as variáveis log do número de veículos e taxa de população urbana  foram
## significativas. E o paâmetro de distpessão 5.95, muito maior que 1 explica a falta de ajuste do modelo de Poisson.

### Reajuste do modelo com seleção de variáveis pelo método stepwise.

m2.1 <- step(m2, direction = "both")

## Resumo do novo modelo


coefic <- data.frame(summary(m2.1)$coefficients)

linha <- rownames(coefic)

coefic <- coefic %>% 
  rename(Estimativa = Estimate, `Erro Padrão` = Std..Error) %>% 
  mutate_at(c("Estimativa", "Erro Padrão"), function(x) round(x, digits = 3)) %>% 
  dplyr::select(-c(z.value, Pr...z..))

rownames(coefic) <- linha

T_3 <- kableExtra::kable(coefic, align = 'cc', booktabs = TRUE) %>% 
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))

## O algoritmo indica que as variáveis log do número de veículos e taxa de população urbana são significativas e 
## tem relação positiva com o número de acidentes de trânsito.

## Agora, vamos realizar o teste da razão de verossimilhança do modelo inicial e do reduzido:
  
anv <- anova(m2, m2.1)
a_nova <-tibble(Modelo = c("Binomial Negativa(Restrito)", "Binomial Negativa(Saturado)"),
           Theta = round(anv$theta, digits = 4), `GL Residual` = anv$`Resid. df`, 
           `Verossimilhança` = anv$`   2 x log-lik.`, `Estatística de teste` = anv$`Pr(Chi)`) %>% 
  mutate(`Estatística de teste` = ifelse(is.na(`Estatística de teste`), "", round(`Estatística de teste`, digits = 4)))

T_4 <- kableExtra::kable(a_nova, align = 'lcccc', booktabs = TRUE) %>% 
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))

## O p-valor do teste foi relativamente alto, portanto pode-se concluir que o modelo restrito se ajusta aos 
## dados amostrais tão bem quanto o modelo considerando todas as covariáveis. Portanto o modelo final fica 
## expresso por:

### Equação do modelo ##

#### Medidas de influência

influenceIndexPlot(m2.1, vars=c("Cook", "Studentized", "hat"), main="Medidas de Influência")

## Com base nesses 3 gráficos, não há indicativos fortes de outliers ou observações influentes.

#### Resíduos quantílicos Aleatorizados

## Outra alternativa para avaliar a qualidade do ajuste é baseada nos resíduos quantílicos aleatorizados. 
## A função qresiduals do pacote statmod extrai este tipo de resíduos do modelo

par(mfrow=c(1,2))

res <- qresiduals(m2.1)

plot(res)

residuos <- qresiduals(m2.1)
qqnorm(residuos)
qqline(residuos, col = 2)

#### Gráficos de efeito

plot(allEffects(m2.1), type = 'response', main = '')

#### Predição

## serão testados 3 perfis de municípios para predição:
## Perfil 1 - log_n_veiculos = 12.5 e pop_urb = 0.5;
## Perfil 2 - log_n_veiculos = 7 e pop_urb = 0.1;
## Perfil 3 - log_n_veiculos = 4 e pop_urb = 0.9;

perfis <- data.frame(log_n_veiculos =  c(12.5, 7, 4), 
                     pop_urb =  c(0.5,  0.1,  0.9))

## Utilizando a função predict para obter o número esperado de acidentes nos perfis:


pred <- floor(predict(m2.1, interval = 'prediction', newdata = perfis, type = 'response'))

perfis$Predi <- pred
rownames(perfis) <- c("Perfil 1", "Perfil 2", "Perfil 3")
colnames(perfis) <- c("Número de Veículos (log)", "Taxa de População Urbana", "Predição")
T_5 <- kableExtra::kable(perfis, align = 'ccc', booktabs = TRUE) %>% 
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))

  