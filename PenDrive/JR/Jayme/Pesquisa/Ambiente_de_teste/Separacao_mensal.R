## Separação mensal dos dados

## Criando os períodos mensais
abril <- head(seq(as.Date("2017-04-17"),     
                  as.Date("2017-05-01"),         
                  by = "1 days"), n = -1)
maio <- head(seq(as.Date("2017-05-01"),     
                 as.Date("2017-06-01"),         
                 by = "1 days"), n = -1)
junho <- head(seq(as.Date("2017-06-01"),     
                  as.Date("2017-07-01"),         
                  by = "1 days"), n = -1)
julho <- head(seq(as.Date("2017-07-01"),     
                  as.Date("2017-08-01"),         
                  by = "1 days"), n = -1)
agosto <- head(seq(as.Date("2017-08-01"),     
                   as.Date("2017-09-01"),         
                   by = "1 days"), n = -1)

#-----------------------------------------------------------------------
## Extração dos tweets nos intervalos
library(jsonlite)

# Informações pessoais.
host <- "http://api.miti.com.br:7863/api/v1"

token <- "edc01a6712ade3b25de804fbdade848d311ef6ec"

# Plaintext com as informações do cliente.

url <- paste0(host, "/profile/info?token=", token)

# GET: /profile/info
#
# Pega as informações pessoais do cadastro do cliente. Está em JSON.

# Retorna uma lista.

doc <- fromJSON(txt = url)
str(doc)

# O número de indentificação do cliente.

idperfil <- doc$data$perfis$id_perfil

# Informações das palavras cadastradas no perfil.

url <- paste0(host, "/profile/", idperfil, "/info?token=", token)
doc <- fromJSON(txt = url)
str(doc)
doc$data$palavras
idpalavra <- "380710"

#-----------------------------------------------------------------------
## Iniciando Extração por período mensal
# Aril
i <- 1
tweets_abril <- list()
while(i <= length(abril)){
    
    dia <- abril[i]
    
    url <- paste0("http://api.miti.com.br:7863",
                  
                  "/api/v1/profile/29312/search/news?",
                  
                  "midias=online",
                  
                  "&id_palavra=", idpalavra,
                  
                  "&date=", dia,
                  
                  "&page=1",
                  
                  "&token=", token)
    
    per_abril <- fromJSON(txt = url)
    
    tweets_abril <- append(tweets_abril,                              
                           sapply(per_abril$data, FUN = "[[",                              
                           "str_conteudo_html"))
    
    i <- i + 1
}
#save.image("Abril.RData")
#save(tweets_abril, file = "Abril.RData")

#-----------------------------------------------------------------------
#Maio (primeira quinzena)
i <- 1
tweets_maio_1 <- list()
while(i <= 15){
    
    dia <- maio[i]
    
    url <- paste0("http://api.miti.com.br:7863",
                  
                  "/api/v1/profile/29312/search/news?",
                  
                  "midias=online",
                  
                  "&id_palavra=", idpalavra,
                  
                  "&date=", dia,
                  
                  "&page=1",
                  
                  "&token=", token)
    
    per_maio <- fromJSON(txt = url)
    
    tweets_maio_1 <- append(tweets_maio_1,                              
                           sapply(per_maio$data, FUN = "[[",                              
                                  "str_conteudo_html"))
    
    i <- i + 1
}
#head(tweets_maio_1)
save(tweets_maio_1, file = "Maio_1.RData")

#Maio (segunda quinzena)

tweets_maio_2 <- list()
while(i <= length(maio)){
    
    dia <- maio[i]
    
    url <- paste0("http://api.miti.com.br:7863",
                  
                  "/api/v1/profile/29312/search/news?",
                  
                  "midias=online",
                  
                  "&id_palavra=", idpalavra,
                  
                  "&date=", dia,
                  
                  "&page=1",
                  
                  "&token=", token)
    
    per_maio <- fromJSON(txt = url)
    
    tweets_maio_2 <- append(tweets_maio_2,                              
                            sapply(per_maio$data, FUN = "[[",                              
                                   "str_conteudo_html"))
    
    i <- i + 1
}
#head(tweets_maio_2)
save(tweets_maio_2, file = "Maio_2.RData")

#-----------------------------------------------------------------------
#Junho (primeira quinzena)
i <- 1
tweets_junho_1 <- list()
while(i <= 15){
    
    dia <- junho[i]
    
    url <- paste0("http://api.miti.com.br:7863",
                  
                  "/api/v1/profile/29312/search/news?",
                  
                  "midias=online",
                  
                  "&id_palavra=", idpalavra,
                  
                  "&date=", dia,
                  
                  "&page=1",
                  
                  "&token=", token)
    
    per_junho <- fromJSON(txt = url)
    
    tweets_junho_1 <- append(tweets_junho_1,                              
                            sapply(per_junho$data, FUN = "[[",                              
                                   "str_conteudo_html"))
    
    i <- i + 1
}
#head(tweets_junho_1)
save(tweets_junho_1, file = "Junho_1.RData")

#Junho (segunda quinzena)

tweets_junho_2 <- list()
while(i <= length(junho)){
    
    dia <- junho[i]
    
    url <- paste0("http://api.miti.com.br:7863",
                  
                  "/api/v1/profile/29312/search/news?",
                  
                  "midias=online",
                  
                  "&id_palavra=", idpalavra,
                  
                  "&date=", dia,
                  
                  "&page=1",
                  
                  "&token=", token)
    
    per_junho <- fromJSON(txt = url)
    
    tweets_junho_2 <- append(tweets_junho_2,                              
                            sapply(per_junho$data, FUN = "[[",                              
                                   "str_conteudo_html"))
    
    i <- i + 1
}
#head(tweets_junho_2)
save(tweets_junho_2, file = "Junho_2.RData")

#-----------------------------------------------------------------------
#Julho (primeira quinzena)
i <- 1
tweets_julho_1 <- list()
while(i <= 15){
    
    dia <- julho[i]
    
    url <- paste0("http://api.miti.com.br:7863",
                  
                  "/api/v1/profile/29312/search/news?",
                  
                  "midias=online",
                  
                  "&id_palavra=", idpalavra,
                  
                  "&date=", dia,
                  
                  "&page=1",
                  
                  "&token=", token)
    
    per_julho <- fromJSON(txt = url)
    
    tweets_julho_1 <- append(tweets_julho_1,                              
                             sapply(per_julho$data, FUN = "[[",                              
                                    "str_conteudo_html"))
    
    i <- i + 1
}
#head(tweets_julho_1)
save(tweets_julho_1, file = "Julho_1.RData")

#Julho (segunda quinzena)

tweets_julho_2 <- list()
while(i <= length(julho)){
    
    dia <- julho[i]
    
    url <- paste0("http://api.miti.com.br:7863",
                  
                  "/api/v1/profile/29312/search/news?",
                  
                  "midias=online",
                  
                  "&id_palavra=", idpalavra,
                  
                  "&date=", dia,
                  
                  "&page=1",
                  
                  "&token=", token)
    
    per_julho <- fromJSON(txt = url)
    
    tweets_julho_2 <- append(tweets_julho_2,                              
                             sapply(per_julho$data, FUN = "[[",                              
                                    "str_conteudo_html"))
    
    i <- i + 1
}
#head(tweets_julho_2)
save(tweets_julho_2, file = "Julho_2.RData")

#-----------------------------------------------------------------------
#Agosto (primeira quinzena)
i <- 1
tweets_agosto_1 <- list()
while(i <= 15){
    
    dia <- agosto[i]
    
    url <- paste0("http://api.miti.com.br:7863",
                  
                  "/api/v1/profile/29312/search/news?",
                  
                  "midias=online",
                  
                  "&id_palavra=", idpalavra,
                  
                  "&date=", dia,
                  
                  "&page=1",
                  
                  "&token=", token)
    
    per_agosto <- fromJSON(txt = url)
    
    tweets_agosto_1 <- append(tweets_agosto_1,                              
                             sapply(per_agosto$data, FUN = "[[",                              
                                    "str_conteudo_html"))
    
    i <- i + 1
}
#head(tweets_agosto_1)
save(tweets_agosto_1, file = "Agosto_1.RData")

#Agosto (segunda quinzena)

tweets_agosto_2 <- list()
while(i <= length(agosto)){
    
    dia <- agosto[i]
    
    url <- paste0("http://api.miti.com.br:7863",
                  
                  "/api/v1/profile/29312/search/news?",
                  
                  "midias=online",
                  
                  "&id_palavra=", idpalavra,
                  
                  "&date=", dia,
                  
                  "&page=1",
                  
                  "&token=", token)
    
    per_agosto <- fromJSON(txt = url)
    
    tweets_agosto_2 <- append(tweets_agosto_2,                              
                             sapply(per_agosto$data, FUN = "[[",                              
                                    "str_conteudo_html"))
    
    i <- i + 1
}
#head(tweets_agosto_2)
save(tweets_agosto_2, file = "Agosto_2.RData")
#-----------------------------------------------------------------------
#tweets_total <- length(tweets_abril) + length(tweets_maio) + 
                #length(tweets_junho)+ length(tweets_julho) + 
                #length(tweets_agosto) 
#tweets_total
#save(tweets_total, file = "Total.RData")
# 6603 tweets

