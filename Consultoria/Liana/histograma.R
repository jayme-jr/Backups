## Pacotes ##

library(graphics)
## Histogramas

## Primeiro baixamos a base
dados <- read.csv2("Banco de dados reduzido.csv", sep = ";", dec = ",")




### Passo 1. construindo o histograma
hist(dados$HA1C)

### Passo 3. escolhendo os intervalos com o argumento 
# breaks = c(início do primeiro intervalo, fim do primeiro e início do segundo, fim do segundo e início do terceiro,...) 
# Nesse caso, para ter os intervalos que você pediu, utilizei
# min(dados$HA1C, na.rm = TRUE), essa função retorna o valor mínimo da coluna HA1C, mas como esse resultado 
# seria NA devido a ter NAs nessa coluna, então usei o argumento "na.rm = TRUE" para que os NAs não 
# sejam levados em consideração na conta do mínimo. O mesmo vale para a função max(dados$HA1C, na.rm = TRUE) que 
# calcula o máximo.
# então para criar os intervalos ficou assim: 
# Início do primeiro intervalo - min(dados$HA1C, na.rm = TRUE);
# fim do primeiro intervalo e início do segundo - 5.7;
# fim do segundo intervalo e início do terceiro - 6.5;
# fim do terceiro intervalo - max(dados$HA1C, na.rm = TRUE).
# por fim os argumento "right = FALSE" para garantir que os intervalos sejam abertos a esquerda e fechados a direita
# e "freq = TRUE" para o eixo y ser a frequência de cada intervalo 
hist(dados$HA1C, breaks = c(min(dados$HA1C, na.rm = TRUE), 5.7, 6.5, max(dados$HA1C, na.rm = TRUE)), right = FALSE, 
     freq = TRUE)

### Passo 3. caso vocẽ não queira que no eixo x apareça todos os números e sim somente a média do intervalo, 
# criar um objeto com essas médias de cada intervalo pedido.

intervalos <- c(min(dados$HA1C, na.rm = TRUE), 5.7, 6.5, max(dados$HA1C, na.rm = TRUE))
med <- (intervalos[-1]+intervalos[-length(intervalos)])*0.5

# para isso precisamos usar o argumento "axes = FALSE" no histograma para apagar os eixos para criar os novos
# agora usamos a função axis(1, med, med) do pacote graphics. O primeiro argumento (1) referencia o eixo 
# 'das abscissas', eixo x, o segundo argumento (med) indica os locais onde você deseja os 'ticks', o terceiro 
# argumento (med) é o vetor com o que vai ser escrito.
# e para recriar o eixo y, usei seq(from = 0, to = length(dados$HA1C), by = 10) para criar o objeto que 
# representará o eixo y. A função "seq()" cria uma sequência de valores, nesse caso foi criada uma sequência
# entre 0 e a quantidade de linhas da base, "from" o valor inicial da sequência, "to" o valor final e 
# "by" o intervalo entre os valores.
eixoy <- seq(from = 0, to = length(dados$HA1C), by = 10)
hist(dados$HA1C, breaks = intervalos, freq = TRUE, axes = FALSE)
axis(1, med, med)
axis(2, eixoy, eixoy)


# Agora colocaremos nomes nos eixos, no gráfico, colorir as barras, colocar bordas nas barras e labels
# no argumento "main" escrevemos a string que será o título, no argumento "xlab" e "ylab" escrevemos as 
# stringa que serão os nomes dos eixos x e y, respectivamente, "labels = TRUE" indica que acima das barras 
# vao aparecer a frequência de cada intervalo, "col" onde colocamos a string com o nome da cor que queremos 
# que preencha as barras e "border" a string com a cor da borda das barras.


hist(dados$HA1C, breaks = intervalos, freq = TRUE, axes = FALSE, main = "Histograma da Variável HA1C", 
     xlab = "Intervalos HA1C", ylab = "Frequência", labels = TRUE, col = "blue", border = "black")
axis(1, med, med)
axis(2, eixoy, eixoy)
