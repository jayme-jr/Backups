
##INSTALANDO PACOTES-------------------------------------------
install.packages("twitteR",
                 dependencies = TRUE,
                 repos = "http://cran-r.c3sl.ufpr.br/")

packageVersion("twitteR")
ls("package:twitteR")

##------------------------------------------------------------
##access_token <- ""
##access_secret <- ""
##PROCESSO DEVE SER REPETIDO TODA VEZ QUE FOR UTILIZAR O SCREIPT!!!
library(twitteR)

consumer_key <- "A6TBC8RSRrCmlZCLv6VKz9g1g"
consumer_secret <- "Vz738FIULh3zr5jvokMaoilciq1pzrjUEkDY1b1PVQLMMtCTDz"
access_token <- "928687529909473281-rEBdmrxLZfNMNUkCeyZqHsFAG001CPg"
access_secret <- "zHZWR7BJRtquHYjj8ZQ1bMCx7fXNpQVCrlubyseEHSQRv"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#OAuthFactory.

#cred <- OAuthFactory$new(consumerKey = consumer_key, consumerSecret = consumer_secret,
                        # requestURL='https://api.twitter.com/oauth/request_token', 
                       #  accessURL='https://api.twitter.com/oauth/access_token',
                        # authURL='https://api.twitter.com/oauth/authorize')

##---------------------------------------------------------

## Procurando por tweets de uma # específica--------------------------
tw <- searchTwitter("#Rstats", n = 100)
head(tw)


## Retirando retweets--------------------------------------------
rtw <- strip_retweets(tw,
                      strip_manual = TRUE,
                      strip_mt = TRUE)
length(rtw)/length(tw)
head(rtw)

## Classe do objeto.
class(tw)

## Estratura de um twitte da lista.---------------------------------
## Pelo `str()` pode-se ver as funções extratoras possíveis
twi <- tw[[10]]
class(twi)
str(twi)

## Cria um vetor com os textos.
r <- sapply(rtw, function(x) x$getText())
r

## Transforma os twittes em tabela
dtw <- twListToDF(rtw)
head(dtw)

## Colher tweets com coordenada central e raio-----------------
twgeo <- searchTwitter("#CoisaDePreto", 
                       geocode = "-25.4505006,-49.2315142,30km")
                        
length(twgeo)

dtwgeo <- twListToDF(twgeo)     
names(dtwgeo)
##Caso queira escolher só alguns tweets
#subset(dtwgeo, select = c(5, 11, 12, 15, 16))

## Mostrar somente os conteúdos dos tweets-------------------------
cat(dtwgeo$text, sep = "\n\n")

##Fazer busca num intervalo de tempo---------------------------
twtime <- searchTwitter("#rstats",
                    since = "2017-11-01",
                    until = "2017-11-04")
head(twtime)

##Informações de Usuários-------------------------------------
u <- getUser("Cogu")
str(u)

# Informações possíveis
u$getDescription()
u$getUrl()
u$getLocation()
u$getName()
u$getCreated()
u$getScreenName()
u$getLastStatus()
u$getProfileImageUrl()

# Número de usuários que segue.
u$getFriendsCount()
u$getFriends(n = 5)

# Número de seguidores e lista dos seguidores.
u$getFollowersCount()
# u$getFollowers()

# Número de likes.
u$getFavouritesCount()

# Linha do tempo de usuário
utm <- userTimeline(u, n = 100)
utm[1:5]

##Tendências-------------------------------------------
a <- availableTrendLocations()
subset(a, country == "Brazil")

closestTrendLocations(lat = -25.4505006, long = -49.2315142)

tr <- getTrends(455822)
head(tr[, 1:2])
