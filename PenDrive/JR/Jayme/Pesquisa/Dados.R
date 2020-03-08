#-----------------------------------------------------------------------

# Carrega pacotes necessários.



library(jsonlite)



#-----------------------------------------------------------------------

# Informações pessoais.



host <- "http://api.miti.com.br:7863/api/v1"

token <- "edc01a6712ade3b25de804fbdade848d311ef6ec"



# Plaintext com as informações do cliente.

url <- paste0(host, "/profile/info?token=", token)

browseURL(url)



#-----------------------------------------------------------------------

# GET: /profile/info

#

# Pega as informações pessoais do cadastro do cliente. Está em JSON.



# Retorna uma lista.

doc <- fromJSON(txt = url)

str(doc)



# O número de indentificação do cliente.

idperfil <- doc$data$perfis$id_perfil



#-----------------------------------------------------------------------

# Informações das palavras cadastradas no perfil.



url <- paste0(host, "/profile/", idperfil, "/info?token=", token)

browseURL(url)



doc <- fromJSON(txt = url)

str(doc)



doc$data$palavras



#-----------------------------------------------------------------------

# ATTENTION: Fazendo busca pela data.



# Consulta com data e paginação.



# Uber.

idpalavra <- "380710"

dia <- "2017-04-17"



url <- paste0("http://api.miti.com.br:7863",
              
              "/api/v1/profile/29312/search/news?",
              
              "midias=online",
              
              "&id_palavra=", idpalavra,
              
              "&date=", dia,
              
              "&page=1",
              
              "&token=", token)



browseURL(url)



doc <- fromJSON(txt = url)

str(doc)



# IMPORTANT: Veja que existe um slot chamado `next_page_url` que pode

# ser usado para fazer as consultas em loop. Basta ir lendo até ter um

# valor NULL nesse campo. Excelente!.

names(doc[[1]]$paginacao)

# Salvando o .RData para cada dia

# save.image("%s.RData")
# salvando o RData dos twitters do dia
# save(doc, file = sprintf("%s.RData", dia))

# Período de coleta dos dados que vai do dia 17/04/2017
# até 31/08/2017(peródo de parceria com a miti). 
periodo <- head(seq(as.Date("2017-04-17"),         
       as.Date("2017-09-01"),         
       by = "1 days"), n = -1)
periodo

# Ctrl + Shift + Enter para rodar o script todo
#-----------------------------------------------------------------------
load("2017-07-31.RData")

candidatos <- c("augusto", "Nilton", "Pedro", "Willian")

sample(candidatos, 2)
