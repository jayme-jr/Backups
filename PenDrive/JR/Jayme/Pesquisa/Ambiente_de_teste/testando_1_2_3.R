#-----------------------------------------------------------------------

# Carrega pacotes necessários.



library(jsonlite)



#-----------------------------------------------------------------------

# Informações pessoais.



host <- "http://api.miti.com.br:7863/api/v1"

token <- "edc01a6712ade3b25de804fbdade848d311ef6ec"



# Plaintext com as informações do cliente.

url <- paste0(host, "/profile/info?token=", token)

#browseURL(url)



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

#browseURL(url)



doc <- fromJSON(txt = url)

str(doc)



doc$data$palavras



#-----------------------------------------------------------------------

# ATTENTION: Fazendo busca pela data.



# Consulta com data e paginação.



# Uber.

idpalavra <- "380710"

dia <- "2017-07-17"



url <- paste0("http://api.miti.com.br:7863",
              
              "/api/v1/profile/29312/search/news?",
              
              "midias=online",
              
              "&id_palavra=", idpalavra,
              
              "&date=", dia,
              
              "&page=1",
              
              "&token=", token)



#browseURL(url)



doc <- fromJSON(txt = url)

str(doc)



# IMPORTANT: Veja que existe um slot chamado `next_page_url` que pode

# ser usado para fazer as consultas em loop. Basta ir lendo até ter um

# valor NULL nesse campo. Excelente!.

names(doc[[1]]$paginacao)

# Salvando o .RData para cada dia

# save.image("%s.RData")
# salvando o RData dos twitters do dia
 #save(doc, file = sprintf("%s_teste.RData", dia))

# Período de coleta dos dados que vai do dia 17/04/2017
# até 31/08/2017(peródo de parceria com a miti). 
# periodo <- head(seq(as.Date("2017-04-17"),         
#                   as.Date("2017-09-01"),         
#                    by = "1 days"), n = -1)
# periodo

# Ctrl + Shift + Enter para rodar o script todo
#-----------------------------------------------------------------------
# load("2017-07-31.RData")

#-----------------------------------------------------------------------
 
# TESTANDO... 1, 2, 3...
 #doc[[2]]
#diaNames <- names(doc[[2]])
#diaNames
#length(diaNames)
#x <- length(diaNames)
#x

#tweets
#doc[[2]][diaNames[1]][str_conteudo_html]
#doc[[2]][diaNames][1]
#diaNames[1]
#doc[[2]][which(doc[[2]]$paginacao == diaNames[1])]

#doc[[1]]$paginacao$next_page_url


#length(doc)

#str(doc[2])
#names(doc$data)

#str(doc[1])



#doc$data[[1]]$str_conteudo_html
#doc$data[[2]]$str_conteudo_html

#walmes[2]
#walmes[[2]]

#"["
#"[["

#precip[2]
## Extraindo o conteúdo das listas
#sapply(doc$data, FUN = "[[", "str_conteudo_html")



#l <- list(page1 = list(tw1 = "a", tw2 = "b"),
 #         page2 = list(tw3 = "c", tw4 = "d"))

#l

#unlist(l, recursive = FALSE)


#doc[[2]]$str_content_html

#lapply
    
 #names(doc[[2]])   
    
#unlist(doc, recursive = FALSE) 

#tweets <- sapply(doc$data, FUN = "[[", "str_conteudo_html")
#tweets
#head(doc)
#x <- unlist(doc)
#tweets <- sapply(doc$data, FUN = "[[", "str_conteudo_html")
#head(x)
#names(x)
#tweets <- sapply(x, FUN = "[", "str_conteudo_html")
#head(tweets)
names(doc[[1]]$paginacao$total)
doc[[1]]$paginacao$last_page

i <- 1
t <- doc[[1]]$paginacao$last_page
i
t
while(i <= t){
    
   print(i) 
    i <- i + 1
}
print(i)


## testes
#str(per_abril)
#per_abril
#dia <- abril[2]
#dia
#x <- unlist(per_abril, recursive = FALSE)
#x
#weets <- sapply(x$data, FUN = "[[", "str_conteudo_html")
#tweets
#sapply(x$data, FUN = "[[", "str_conteudo_html")
#tail(tweets_abril)
#save(tweets_abril, file = "Abril.RData")
length(tweets_agosto)


