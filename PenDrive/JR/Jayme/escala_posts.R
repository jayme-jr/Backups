library(knitr)
library(kableExtra)

post_Authors
post_Authors <- post_Authors[-7]

autores <- sample(post_Authors, replace = FALSE)
autores
corretores <- c("Jhenifer", "Nilton", "Altamiro", "William", "Augusto",
                "Jayme", "Walmes", "William2", "Vinicius", "Nivea", "Lineu")

datas <- c("21/08", "28/08", "04/09", "11/09", "18/09", "25/09",
           "09/10", "16/10", "23/10", "30/10", "06/11")

issues <- c("issue #95", "issue #96", "issue #97", "issue #98", "issue #99", "issue #100", 
            "issue #101", "issue #102", "issue #103", "issue #104", "issue #105")

escala <- data.frame(autores, datas, corretores, issues)


kableExtra::
  kable(escala, "markdown", col.names = c("Autor", "Entrega Para Revisão", 
                                                    "Revisor", "Status"),
        align = "c")
