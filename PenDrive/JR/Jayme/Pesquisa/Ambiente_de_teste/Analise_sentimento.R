# Lendo o dicionário léxico de sentimentos.

sent <- read.table("/home/petest/Área de Trabalho/JR/Jayme/Pesquisa/Ambiente_de_teste/lexico_v3.0.txt",
                   header = FALSE,
                   sep = ",",
                   quote = "",
                   stringsAsFactors = FALSE)
names(sent) <- c("term", "class", "pol", "ann")
head(sent)

library(tm)
#-----------------------------------------------------------------------
# Criando o Corpus de Abril.

cps_abril <- Corpus(VectorSource(tweets_abril),
                    readerControl = list(language = "pt"))
head(cps_abril)

cps_abril
# Salvando o corpus de Abril
save(cps_abril, file = "corpus_Abril.RData")

# Fazendo as operações de limpeza.
cps_abril <- tm_map(cps_abril, FUN = content_transformer(tolower))
cps_abril <- tm_map(cps_abril, FUN = removePunctuation)
cps_abril <- tm_map(cps_abril, FUN = removeNumbers)
cps_abril <- tm_map(cps_abril, FUN = stripWhitespace)

# Criando a matriz de documentos e termos
dtm_abril <- DocumentTermMatrix(cps_abril)

dtm_abril

#-----------------------------------------------------------------------
# Intersecção entre os termos do corpus e o léxico. 
inter_abril <- intersect(x = Terms(dtm_abril),
                   y = sent$term)
length(inter_abril)

# Obter o vetor de polaridades associada o vetor de léxicos.
lex_abril <- merge(x = data.frame(term = inter_abril,
                            stringsAsFactors = FALSE),
             y = sent,
             sort = FALSE)
str(lex_abril)
# Remover os termos na dtm_abril que não tem polaridade.
m_abril <- as.matrix(dtm_abril)
m_abril <- m_abril[, lex_abril$term]

ncol(m_abril) == nrow(lex_abril)

all(colnames(m_abril) == lex_abril$term)

# Soma aritmética das polaridades por termo em cada documento.
soma_abril <- m_abril %*% cbind(lex_abril$pol)

total_abril <- sum(soma_abril[,1])

# Salvando o objeto com os valores da análise de sentimento 
# de cada documento de Abril
save(soma_abril, file = "sentimento_Aril.RData")

# Salvando o resultado total da análise de sentimento de Abril
save(total_abril, file = "resultado_Abril.RData")

#-----------------------------------------------------------------------
# Criando o Corpus de Maio (primeira quinzena).

cps_maio_1 <- Corpus(VectorSource(tweets_maio_1),
                    readerControl = list(language = "pt"))
head(cps_maio_1)

cps_maio_1
# Salvando o corpus da primeira quinzena de Maio
save(cps_maio_1, file = "corpus_Maio_1.RData")

# Fazendo as operações de limpeza.
cps_maio_1 <- tm_map(cps_maio_1, FUN = content_transformer(tolower))
cps_maio_1 <- tm_map(cps_maio_1, FUN = removePunctuation)
cps_maio_1 <- tm_map(cps_maio_1, FUN = removeNumbers)
cps_maio_1 <- tm_map(cps_maio_1, FUN = stripWhitespace)

# Criando a matriz de documentos e termos
dtm_maio_1 <- DocumentTermMatrix(cps_maio_1)

dtm_maio_1

#-----------------------------------------------------------------------
# Intersecção entre os termos do corpus e o léxico. 
inter_maio_1 <- intersect(x = Terms(dtm_maio_1),
                         y = sent$term)
length(inter_maio_1)

# Obter o vetor de polaridades associada o vetor de léxicos.
lex_maio_1 <- merge(x = data.frame(term = inter_maio_1,
                                  stringsAsFactors = FALSE),
                   y = sent,
                   sort = FALSE)
str(lex_maio_1)
# Remover os termos na dtm_maio_1 que não tem polaridade.
m_maio_1 <- as.matrix(dtm_maio_1)
m_maio_1 <- m_maio_1[, lex_maio_1$term]

ncol(m_maio_1) == nrow(lex_maio_1)

all(colnames(m_maio_1) == lex_maio_1$term)

# Soma aritmética das polaridades por termo em cada documento.
soma_maio_1 <- m_maio_1 %*% cbind(lex_maio_1$pol)

total_maio_1 <- sum(soma_maio_1[,1])
#total_maio_1
# Salvando o objeto com os valores da análise de sentimento 
# de cada documento da primeira quinzena de Maio
save(soma_maio_1, file = "sentimento_Maio_1.RData")

# Salvando o resultado total da análise de sentimento da
# primeira quinzena de Maio
save(total_maio_1, file = "resultado_Maio_1.RData")


# Criando o Corpus de Maio (segunda quinzena).

cps_maio_2 <- Corpus(VectorSource(tweets_maio_2),
                     readerControl = list(language = "pt"))
head(cps_maio_2)

cps_maio_2
# Salvando o corpus da segunda quinzena de Maio
save(cps_maio_2, file = "corpus_Maio_2.RData")

# Fazendo as operações de limpeza.
cps_maio_2 <- tm_map(cps_maio_2, FUN = content_transformer(tolower))
cps_maio_2 <- tm_map(cps_maio_2, FUN = removePunctuation)
cps_maio_2 <- tm_map(cps_maio_2, FUN = removeNumbers)
cps_maio_2 <- tm_map(cps_maio_2, FUN = stripWhitespace)

# Criando a matriz de documentos e termos
dtm_maio_2 <- DocumentTermMatrix(cps_maio_2)

dtm_maio_2

#-----------------------------------------------------------------------
# Intersecção entre os termos do corpus e o léxico. 
inter_maio_2 <- intersect(x = Terms(dtm_maio_2),
                          y = sent$term)
length(inter_maio_2)

# Obter o vetor de polaridades associada o vetor de léxicos.
lex_maio_2 <- merge(x = data.frame(term = inter_maio_2,
                                   stringsAsFactors = FALSE),
                    y = sent,
                    sort = FALSE)
str(lex_maio_2)
# Remover os termos na dtm_maio_2 que não tem polaridade.
m_maio_2 <- as.matrix(dtm_maio_2)
m_maio_2 <- m_maio_2[, lex_maio_2$term]

ncol(m_maio_2) == nrow(lex_maio_2)

all(colnames(m_maio_2) == lex_maio_2$term)

# Soma aritmética das polaridades por termo em cada documento.
soma_maio_2 <- m_maio_2 %*% cbind(lex_maio_2$pol)

total_maio_2 <- sum(soma_maio_2[,1])
#total_maio_1
# Salvando o objeto com os valores da análise de sentimento 
# de cada documento da segunda quinzena de Maio
save(soma_maio_2, file = "sentimento_Maio_2.RData")

# Salvando o resultado total da análise de sentimento da
# segunda quinzena de Maio
save(total_maio_2, file = "resultado_Maio_2.RData")

#-----------------------------------------------------------------------
# Criando o Corpus de Junho (primeira quinzena).

cps_junho_1 <- Corpus(VectorSource(tweets_junho_1),
                     readerControl = list(language = "pt"))
head(cps_junho_1)

cps_junho_1
# Salvando o corpus da primeira quinzena de Junho
save(cps_junho_1, file = "corpus_Junho_1.RData")

# Fazendo as operações de limpeza.
cps_junho_1 <- tm_map(cps_junho_1, FUN = content_transformer(tolower))
cps_junho_1 <- tm_map(cps_junho_1, FUN = removePunctuation)
cps_junho_1 <- tm_map(cps_junho_1, FUN = removeNumbers)
cps_junho_1 <- tm_map(cps_junho_1, FUN = stripWhitespace)

# Criando a matriz de documentos e termos
dtm_junho_1 <- DocumentTermMatrix(cps_junho_1)

dtm_junho_1

#-----------------------------------------------------------------------
# Intersecção entre os termos do corpus e o léxico. 
inter_junho_1 <- intersect(x = Terms(dtm_junho_1),
                          y = sent$term)
length(inter_junho_1)

# Obter o vetor de polaridades associada o vetor de léxicos.
lex_junho_1 <- merge(x = data.frame(term = inter_junho_1,
                                   stringsAsFactors = FALSE),
                    y = sent,
                    sort = FALSE)
str(lex_junho_1)
# Remover os termos na dtm_junho_1 que não tem polaridade.
m_junho_1 <- as.matrix(dtm_junho_1)
m_junho_1 <- m_junho_1[, lex_junho_1$term]

ncol(m_junho_1) == nrow(lex_junho_1)

all(colnames(m_junho_1) == lex_junho_1$term)

# Soma aritmética das polaridades por termo em cada documento.
soma_junho_1 <- m_junho_1 %*% cbind(lex_junho_1$pol)

total_junho_1 <- sum(soma_junho_1[,1])
total_junho_1
# Salvando o objeto com os valores da análise de sentimento 
# de cada documento da primeira quinzena de Junho
save(soma_junho_1, file = "sentimento_Junho_1.RData")

# Salvando o resultado total da análise de sentimento da
# primeira quinzena de Junho
save(total_junho_1, file = "resultado_Junho_1.RData")


# Criando o Corpus de Junho (segunda quinzena).

cps_junho_2 <- Corpus(VectorSource(tweets_junho_2),
                      readerControl = list(language = "pt"))
head(cps_junho_2)

cps_junho_2
# Salvando o corpus da segunda quinzena de Junho
save(cps_junho_2, file = "corpus_Junho_2.RData")

# Fazendo as operações de limpeza.
cps_junho_2 <- tm_map(cps_junho_2, FUN = content_transformer(tolower))
cps_junho_2 <- tm_map(cps_junho_2, FUN = removePunctuation)
cps_junho_2 <- tm_map(cps_junho_2, FUN = removeNumbers)
cps_junho_2 <- tm_map(cps_junho_2, FUN = stripWhitespace)

# Criando a matriz de documentos e termos
dtm_junho_2 <- DocumentTermMatrix(cps_junho_2)

dtm_junho_2

#-----------------------------------------------------------------------
# Intersecção entre os termos do corpus e o léxico. 
inter_junho_2 <- intersect(x = Terms(dtm_junho_2),
                           y = sent$term)
length(inter_junho_2)

# Obter o vetor de polaridades associada o vetor de léxicos.
lex_junho_2 <- merge(x = data.frame(term = inter_junho_2,
                                    stringsAsFactors = FALSE),
                     y = sent,
                     sort = FALSE)
str(lex_junho_2)
# Remover os termos na dtm_junho_2 que não tem polaridade.
m_junho_2 <- as.matrix(dtm_junho_2)
m_junho_2 <- m_junho_2[, lex_junho_2$term]

ncol(m_junho_2) == nrow(lex_junho_2)

all(colnames(m_junho_2) == lex_junho_2$term)

# Soma aritmética das polaridades por termo em cada documento.
soma_junho_2 <- m_junho_2 %*% cbind(lex_junho_2$pol)

total_junho_2 <- sum(soma_junho_2[,1])
total_junho_2
# Salvando o objeto com os valores da análise de sentimento 
# de cada documento da segunda quinzena de Junho
save(soma_junho_2, file = "sentimento_Junho_2.RData")

# Salvando o resultado total da análise de sentimento da
# segunda quinzena de Junho
save(total_junho_2, file = "resultado_Junho_2.RData")

#-----------------------------------------------------------------------
# Criando o Corpus de Julho (primeira quinzena).

cps_julho_1 <- Corpus(VectorSource(tweets_julho_1),
                      readerControl = list(language = "pt"))
head(cps_julho_1)

cps_julho_1
# Salvando o corpus da primeira quinzena de Julho
save(cps_julho_1, file = "corpus_Julho_1.RData")

# Fazendo as operações de limpeza.
cps_julho_1 <- tm_map(cps_julho_1, FUN = content_transformer(tolower))
cps_julho_1 <- tm_map(cps_julho_1, FUN = removePunctuation)
cps_julho_1 <- tm_map(cps_julho_1, FUN = removeNumbers)
cps_julho_1 <- tm_map(cps_julho_1, FUN = stripWhitespace)

# Criando a matriz de documentos e termos
dtm_julho_1 <- DocumentTermMatrix(cps_julho_1)

dtm_julho_1

#-----------------------------------------------------------------------
# Intersecção entre os termos do corpus e o léxico. 
inter_julho_1 <- intersect(x = Terms(dtm_julho_1),
                           y = sent$term)
length(inter_julho_1)

# Obter o vetor de polaridades associada o vetor de léxicos.
lex_julho_1 <- merge(x = data.frame(term = inter_julho_1,
                                    stringsAsFactors = FALSE),
                     y = sent,
                     sort = FALSE)
str(lex_julho_1)
# Remover os termos na dtm_julho_1 que não tem polaridade.
m_julho_1 <- as.matrix(dtm_julho_1)
m_julho_1 <- m_julho_1[, lex_julho_1$term]

ncol(m_julho_1) == nrow(lex_julho_1)

all(colnames(m_julho_1) == lex_julho_1$term)

# Soma aritmética das polaridades por termo em cada documento.
soma_julho_1 <- m_julho_1 %*% cbind(lex_julho_1$pol)

total_julho_1 <- sum(soma_julho_1[,1])
total_julho_1
# Salvando o objeto com os valores da análise de sentimento 
# de cada documento da primeira quinzena de Julho
save(soma_julho_1, file = "sentimento_Julho_1.RData")

# Salvando o resultado total da análise de sentimento da
# primeira quinzena de Julho
save(total_julho_1, file = "resultado_Julho_1.RData")


# Criando o Corpus de Julho (segunda quinzena).

cps_julho_2 <- Corpus(VectorSource(tweets_julho_2),
                      readerControl = list(language = "pt"))
head(cps_julho_2)

cps_julho_2
# Salvando o corpus da segunda quinzena de Julho
save(cps_julho_2, file = "corpus_Julho_2.RData")

# Fazendo as operações de limpeza.
cps_julho_2 <- tm_map(cps_julho_2, FUN = content_transformer(tolower))
cps_julho_2 <- tm_map(cps_julho_2, FUN = removePunctuation)
cps_julho_2 <- tm_map(cps_julho_2, FUN = removeNumbers)
cps_julho_2 <- tm_map(cps_julho_2, FUN = stripWhitespace)

# Criando a matriz de documentos e termos
dtm_julho_2 <- DocumentTermMatrix(cps_julho_2)

dtm_julho_2

#-----------------------------------------------------------------------
# Intersecção entre os termos do corpus e o léxico. 
inter_julho_2 <- intersect(x = Terms(dtm_julho_2),
                           y = sent$term)
length(inter_julho_2)

# Obter o vetor de polaridades associada o vetor de léxicos.
lex_julho_2 <- merge(x = data.frame(term = inter_julho_2,
                                    stringsAsFactors = FALSE),
                     y = sent,
                     sort = FALSE)
str(lex_julho_2)
# Remover os termos na dtm_julho_2 que não tem polaridade.
m_julho_2 <- as.matrix(dtm_julho_2)
m_julho_2 <- m_julho_2[, lex_julho_2$term]

ncol(m_julho_2) == nrow(lex_julho_2)

all(colnames(m_julho_2) == lex_julho_2$term)

# Soma aritmética das polaridades por termo em cada documento.
soma_julho_2 <- m_julho_2 %*% cbind(lex_julho_2$pol)

total_julho_2 <- sum(soma_julho_2[,1])
total_julho_2
# Salvando o objeto com os valores da análise de sentimento 
# de cada documento da segunda quinzena de Julho
save(soma_julho_2, file = "sentimento_Julho_2.RData")

# Salvando o resultado total da análise de sentimento da
# segunda quinzena de Julho
save(total_julho_2, file = "resultado_Julho_2.RData")

#-----------------------------------------------------------------------
# Criando o Corpus de Agosto (primeira quinzena).

cps_agosto_1 <- Corpus(VectorSource(tweets_agosto_1),
                      readerControl = list(language = "pt"))
head(cps_agosto_1)

cps_agosto_1
# Salvando o corpus da primeira quinzena de Agosto
save(cps_agosto_1, file = "corpus_Agosto_1.RData")

# Fazendo as operações de limpeza.
cps_agosto_1 <- tm_map(cps_agosto_1, FUN = content_transformer(tolower))
cps_agosto_1 <- tm_map(cps_agosto_1, FUN = removePunctuation)
cps_agosto_1 <- tm_map(cps_agosto_1, FUN = removeNumbers)
cps_agosto_1 <- tm_map(cps_agosto_1, FUN = stripWhitespace)

# Criando a matriz de documentos e termos
dtm_agosto_1 <- DocumentTermMatrix(cps_agosto_1)

dtm_agosto_1

#-----------------------------------------------------------------------
# Intersecção entre os termos do corpus e o léxico. 
inter_agosto_1 <- intersect(x = Terms(dtm_agosto_1),
                           y = sent$term)
length(inter_agosto_1)

# Obter o vetor de polaridades associada o vetor de léxicos.
lex_agosto_1 <- merge(x = data.frame(term = inter_agosto_1,
                                    stringsAsFactors = FALSE),
                     y = sent,
                     sort = FALSE)
str(lex_agosto_1)
# Remover os termos na dtm_julho_1 que não tem polaridade.
m_agosto_1 <- as.matrix(dtm_agosto_1)
m_agosto_1 <- m_agosto_1[, lex_agosto_1$term]

ncol(m_agosto_1) == nrow(lex_agosto_1)

all(colnames(m_agosto_1) == lex_agosto_1$term)

# Soma aritmética das polaridades por termo em cada documento.
soma_agosto_1 <- m_agosto_1 %*% cbind(lex_agosto_1$pol)

total_agosto_1 <- sum(soma_agosto_1[,1])
total_agosto_1
# Salvando o objeto com os valores da análise de sentimento 
# de cada documento da primeira quinzena de Agosto
save(soma_agosto_1, file = "sentimento_Agosto_1.RData")

# Salvando o resultado total da análise de sentimento da
# primeira quinzena de Agosto
save(total_agosto_1, file = "resultado_Agosto_1.RData")


# Criando o Corpus de Agosto (segunda quinzena).

cps_agosto_2 <- Corpus(VectorSource(tweets_agosto_2),
                       readerControl = list(language = "pt"))
head(cps_agosto_2)

cps_agosto_2
# Salvando o corpus da segunda quinzena de Agosto
save(cps_agosto_2, file = "corpus_Agosto_2.RData")

# Fazendo as operações de limpeza.
cps_agosto_2 <- tm_map(cps_agosto_2, FUN = content_transformer(tolower))
cps_agosto_2 <- tm_map(cps_agosto_2, FUN = removePunctuation)
cps_agosto_2 <- tm_map(cps_agosto_2, FUN = removeNumbers)
cps_agosto_2 <- tm_map(cps_agosto_2, FUN = stripWhitespace)

# Criando a matriz de documentos e termos
dtm_agosto_2 <- DocumentTermMatrix(cps_agosto_2)

dtm_agosto_2

#-----------------------------------------------------------------------
# Intersecção entre os termos do corpus e o léxico. 
inter_agosto_2 <- intersect(x = Terms(dtm_agosto_2),
                            y = sent$term)
length(inter_agosto_2)

# Obter o vetor de polaridades associada o vetor de léxicos.
lex_agosto_2 <- merge(x = data.frame(term = inter_agosto_2,
                                     stringsAsFactors = FALSE),
                      y = sent,
                      sort = FALSE)
str(lex_agosto_2)
# Remover os termos na dtm_agosto_2 que não tem polaridade.
m_agosto_2 <- as.matrix(dtm_agosto_2)
m_agosto_2 <- m_agosto_2[, lex_agosto_2$term]

ncol(m_agosto_2) == nrow(lex_agosto_2)

all(colnames(m_agosto_2) == lex_agosto_2$term)

# Soma aritmética das polaridades por termo em cada documento.
soma_agosto_2 <- m_agosto_2 %*% cbind(lex_agosto_2$pol)

total_agosto_2 <- sum(soma_agosto_2[,1])
total_agosto_2
# Salvando o objeto com os valores da análise de sentimento 
# de cada documento da segunda quinzena de Agosto
save(soma_agosto_2, file = "sentimento_Agosto_2.RData")

# Salvando o resultado total da análise de sentimento da
# primeira quinzena de Agosto
save(total_agosto_2, file = "resultado_Agosto_2.RData")

save.image("Sentimento_Def.RData")

# Gráfico de paretto
# Calculando a soma dos termos mais repetidos de cada período
frq_abril <- slam::colapply_simple_triplet_matrix(dtm_abril, FUN = sum)
frq_abril <- sort(frq_abril, decreasing = TRUE)

frq_maio_1 <- slam::colapply_simple_triplet_matrix(dtm_maio_1, FUN = sum)
frq_maio_1 <- sort(frq_maio_1, decreasing = TRUE)

frq_maio_2 <- slam::colapply_simple_triplet_matrix(dtm_maio_2, FUN = sum)
frq_maio_2 <- sort(frq_maio_2, decreasing = TRUE)

frq_junho_1 <- slam::colapply_simple_triplet_matrix(dtm_junho_1, FUN = sum)
frq_junho_1 <- sort(frq_junho_1, decreasing = TRUE)

frq_junho_2 <- slam::colapply_simple_triplet_matrix(dtm_junho_2, FUN = sum)
frq_junho_2 <- sort(frq_junho_2, decreasing = TRUE)

frq_julho_1 <- slam::colapply_simple_triplet_matrix(dtm_julho_1, FUN = sum)
frq_julho_1 <- sort(frq_julho_1, decreasing = TRUE)

frq_julho_2 <- slam::colapply_simple_triplet_matrix(dtm_julho_2, FUN = sum)
frq_julho_2 <- sort(frq_julho_2, decreasing = TRUE)

frq_agosto_1 <- slam::colapply_simple_triplet_matrix(dtm_agosto_1, FUN = sum)
frq_agosto_1 <- sort(frq_agosto_1, decreasing = TRUE)

frq_agosto_2 <- slam::colapply_simple_triplet_matrix(dtm_agosto_2, FUN = sum)
frq_agosto_2 <- sort(frq_agosto_2, decreasing = TRUE)


# tENTATIVA DE FAZER UM GRÁFICO DE PARETTO PARA O PERÍODO TODO(NÃO DEU CERTO)
frq_total <- c(head(frq_abril, n = 20), head(frq_maio_1, n = 20), 
               head(frq_maio_2, n = 20), head(frq_junho_1, n = 20),
               head(frq_junho_2, n = 20), head(frq_julho_1, n = 20), 
               head(frq_julho_2, n = 20), head(frq_agosto_1, n = 20), 
               head(frq_agosto_2, n = 20))

frq_total <- DocumentTermMatrix(frq_total)
str(frq_total)
frq_total <- slam::as.simple_triplet_matrix(frq_total)
frq_total <- slam::colapply_simple_triplet_matrix(frq_total, FUN = sum)
frq_total <- sort(frq_total, decreasing = TRUE)
head(frq_total, n = 20)
#-----------------------------------------------------------------------

library(lattice)
# Gráfico de Paretto de cada período
barchart(head(frq_abril, n = 20), xlim = c(0, NA),
         main = "TERMOS MAIS REPETIDOS NO MÊS DE ABRIL",
         xlab = "Nr de repetições do termo",
         ylab = "Termos")

barchart(head(frq_maio_1, n = 20), xlim = c(0, NA),
         main = "TERMOS MAIS REPETIDOS NA PRIMEIRA QUINZENA DE MAIO",
         xlab = "Nr de repetições do termo",
         ylab = "Termos")

barchart(head(frq_maio_2, n = 20), xlim = c(0, NA),
         main = "TERMOS MAIS REPETIDOS NA SEGUNDA QUINZENA DE MAIO",
         xlab = "Nr de repetições do termo",
         ylab = "Termos")

barchart(head(frq_junho_1, n = 20), xlim = c(0, NA),
         main = "TERMOS MAIS REPETIDOS NA PRIMEIRA QUINZENA DE JUNHO",
         xlab = "Nr de repetições do termo",
         ylab = "Termos")

barchart(head(frq_junho_2, n = 20), xlim = c(0, NA),
         main = "TERMOS MAIS REPETIDOS NA SEGUNDA QUINZENA DE JUNHO",
         xlab = "Nr de repetições do termo",
         ylab = "Termos")

barchart(head(frq_julho_1, n = 20), xlim = c(0, NA),
         main = "TERMOS MAIS REPETIDOS NA PRIMEIRA QUINZENA DE JULHO",
         xlab = "Nr de repetições do termo",
         ylab = "Termos")

barchart(head(frq_julho_2, n = 20), xlim = c(0, NA),
         main = "TERMOS MAIS REPETIDOS NA SEGUNDA QUINZENA DE JULHO",
         xlab = "Nr de repetições do termo",
         ylab = "Termos")

barchart(head(frq_agosto_1, n = 20), xlim = c(0, NA),
         main = "TERMOS MAIS REPETIDOS NA PRIMEIRA QUINZENA DE AGOSTO",
         xlab = "Nr de repetições do termo",
         ylab = "Termos")

barchart(head(frq_agosto_2, n = 20), xlim = c(0, NA),
         main = "TERMOS MAIS REPETIDOS NA SEGUNDA QUINZENA DE AGOSTO",
         xlab = "Nr de repetições do termo",
         ylab = "Termos")

# Criando a matriz dos resultados da análise de sentimento
results <- c(total_abril, total_maio_1 + total_maio_2,
             total_junho_1 + total_junho_2, 
             total_julho_1 + total_julho_2, 
             total_agosto_1 + total_agosto_2)
results <- as.matrix(results)
rownames(results) <- c("Abril", "Maio", "Junho", "Julho", "Agosto")
plot(results, type = "l", 
     main = "Resultado da Análise de sentimento ao Longo dos Meses", 
     xlab = "Meses", ylab = "Score da Análise")
as.data.frame(results)     
teste_grafico <- ts(results)
str(results)

teste_grafico
str(teste_grafico)
row.names(teste_grafico) <- c("Abril", "Maio", "Junho", "Julho", "Agosto")

xyplot(teste_grafico, 
       main = "Resultado da Análise de sentimento ao Longo dos Meses", 
       xlab = "Meses", ylab = "Score da Análise")

