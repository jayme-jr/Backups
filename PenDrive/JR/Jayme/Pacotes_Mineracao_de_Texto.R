install.packages("XML",
                 dependencies = TRUE,
                 repos = "http://cran-r.c3sl.ufpr.br/")
library(XML)
library(RCurl)
library(jsonlite)
install.packages("twitteR",
                 dependencies = TRUE,
                 repos = "http://cran-r.c3sl.ufpr.br/")
library(twitteR)
packageVersion("twitteR")
ls("package:twitteR")



# # Install Selenium Ubuntu 16.04.
system("apt-cache search selenium")

# # Baixar o Selenium Stand Alone.
 browseURL("http://selenium-release.storage.googleapis.com/index.html")

 # # 3.0/selenium-server-standalone-3.0.1.jar
 url <- paste0("http://selenium-release.storage.googleapis.com/",
                "3.0/selenium-server-standalone-3.0.1.jar")
 
 # # Baixa o arquivo .jar para instalar o Selenium.
 download.file(url = url,
                destfile = paste0("~/Downloads/", basename(url)))
 
 # # Verifica presença do arquivo baixado.
  dir(path = "~/Downloads", pattern = "*.jar")
  
  # #-----------------------------------------------------------------------
  #
  cmd <- sprintf("java -jar %s", basename(url))
  cmd
  #
  #-----------------------------------------------------------------------
  
  # https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-basics.html
  
  install.packages("RSelenium")
  
  library(RSelenium)
  
  help(rsDriver, h = "html")
  
  # Essa função instala o Selenium quando chamada pela primeira vez.
  rD <- rsDriver(browser = "firefox")
  # Depois de instalar/atualizar, uma instância do navegador abre.
  
  