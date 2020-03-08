## Pacotes ##

library(graphics)

dados <- read.csv2("Banco de dados reduzido.csv", sep = ";", dec = ",")

## Dúvidas

# 1
# A primeira coisa que se deve fazer para criar seus próprios eixos no histograma é usar o argumento 
# "axes = FALSE", assim você apaga os eixos criados automaticamente pela função "hist()"
# Vou mostrar encurtando o código criando objetos com os breaks e outros necessários pra deixar 
# o código mais visível
intervalos <- c(min(dados$HA1C, na.rm = TRUE),6.0,6.5, max(dados$HA1C, na.rm = TRUE))

hist(dados$HA1C, breaks = intervalos, right = FALSE, freq=TRUE, 
     main = "Histograma da variável HbA1c", xlab = "Intervalos de HbA1c", ylab = "Frequência", 
     labels = TRUE, axes = FALSE)
# Repare no último argumento da função e como o gráfico vem sem os eixos. A oartir daqui você usa
# o pacote "graphics" para criar os eixos como você quer
# Explicando a função "axis()":
# o primeiro argumento você já entendeu;
# o segundo argumento representa onde no eixo serão escritos os valores;
# o terceiro argumento recebe o que será escrito nos pontos escolhidos pelo segundo argumento
# Vou fazer um exemplo mais didático pra facilitar a compreensão;

## EX:
# digamos que eu queira que os pontos onde serão colocados os valores no eixo sejam os breaks:
tics <- c(min(dados$HA1C, na.rm = TRUE),6.0,6.5, max(dados$HA1C, na.rm = TRUE))
# então os pontos do eixo que receberão os valores serão 4.5, 6, 6.5 e 10.8
# E quero que no eixo apareçam as letras A, B, C e D
valores <- c("A", "B", "C", "D")
# OBS: esses dois vetores idealmente devem ter o mesmo tamanho pra evitar problemas
hist(dados$HA1C, breaks = intervalos, right = FALSE, freq=TRUE, 
     main = "Histograma da variável HbA1c", xlab = "Intervalos de HbA1c", ylab = "Frequência", 
     labels = TRUE, axes = FALSE)
axis(1, tics, valores)
# Então o segundo argumento é onde você quer que os valores apareçam e o terceiro é o que você
# quer que apareça no eixo
# o mesmo vale para o eixo y
# portanto para escrever como você pediu seria assim:
## Passo 1: criar os objetos necessários para o histograma e os eixos
# Breaks do histograma
intervalos <- c(min(dados$HA1C, na.rm = TRUE),6.0,6.5, max(dados$HA1C, na.rm = TRUE))
# Tics do eixo x (os valores que você propôs como exemplo não são possíveis aqui, pois na base que tenho,
# o mínimo é 4.5 e o máximo é 10.8, então usarei eles, então usarei outros como exemplo)
tics_eixox <- c(min(dados$HA1C, na.rm = TRUE),5.7,6.5, 9, max(dados$HA1C, na.rm = TRUE))
# Os valores que aparecerão no eixo() geralmente se usa o mesmo que os pontos dos tics porque
# se quer que os valores escolhidos como tic apareçam no eixo, mas como no exemplo anterior é
# possível que sejam diferentes, mas nesse caso serão os mesmos)
valores_eixox <- tics_eixox
# Vocẽ acertou a interpretação da sequência no eixo y, mas no exemplo não posso fazer igual porque
# a base que tenho é menor
# tics e valores do eixo y
eixoy <- seq(from = 0, to = length(dados$HA1C), by = 10)
## Passo 2: criar o histrograma sem esquecer de elimiar os eixos criados automaticamente usando
# o argumento "axes = FALSE" e depois usar a função "axis()" para criar os eixos como abaixo.
# OBS: não esquecer de carregar o pacote "graphics", caso contrário a função "axis()" não funcionará.
# Caso você não tenha o pacote instalado, basta usar "install.packages(graphics, dependencies = TRUE)"
# e posteriormente "library(graphics)" para carregar o pacote
hist(dados$HA1C, breaks = intervalos, right = FALSE, freq=TRUE, 
     main = "Histograma da variável HbA1c", xlab = "Intervalos de HbA1c", ylab = "Frequência", 
     labels = TRUE, axes = FALSE)
axis(1, tics_eixox, valores_eixox)
axis(2, eixoy, eixoy)
# OBS: A parte de criar objetos antes é uma recomendação minha para manter o código mais simples e 
# mais fácil de modificar caso necessário

## 2
# 2.1
# no excel o critério é o seguinte se ele encontra valores iguais ele mantém o que foi encontrado primeiro
# e no excel ele busca de cima para baixo na coluna, então ele mantém o valos mais acima
# no R isso é possível com a função "unique()"
# como no exemplo a seguir, se você simplesmente fizer "unique(x)" ele vai fazer exatamente como no excel, 
# vai manter o valor encontrado primeiro, mas você pode usar o argumento "fromLast = TRUE" para que ele mantgenha
# o ultimo encontrado
x <- c(3:5, 11:8, 8:13)
ux <- unique(x)
u2 <- unique(x, fromLast = TRUE)
# Mas acredito que para o seu caso o do excel já resolve seu problema

# 2.2
# Esse valor baixo representa a raridade do valor que você viu em relação a hipótese de que não existe 
# correlação.
# o teste funciona assim, ele parte da hipótese de  que não existe correlação(H0) com confiança de 95%(0.95),
# então se o p-valor for menor que 0.05, significa que você rejeita a hipótese nula(H0), então esse valor
# tão pequeno mostra o quanto a probabilidade de H0 ser verdadeira é pquena, então vocẽ conclui que
# com 95% de confiança existe correlação

# 2.3, 2.4 e 2.5
# infelizmente essas não consigo responder agora, sei que uma abordagem boa é análise categórica, 
# mas vou precisar estudar um pouco isso para não correr o risco de te falar bobagem 
# (já estou com o livro em mãos)

# 2.6
# Quanto ao boxplot, realmente eles mostraram resultados muito diferentes, tanto média, mediana e variância
# não sei exatamente como explicar essa diferença, mas fiz um teste e acredito que a diferença possa se dar
# ao tratamento dos NAs que no R ele já lida com eles no box-plot como mostrado abaixo onde fiz dois box-plots 
# lado a lado um sem tirar os NAs e outo tirando os NAs e ficaram iguais. Tavez o stata trate diferente os NAs.
# MAs eu não conheço o Stata para confirmar
boxplot(dados$HA1C, na.omit(dados$HA1C))
