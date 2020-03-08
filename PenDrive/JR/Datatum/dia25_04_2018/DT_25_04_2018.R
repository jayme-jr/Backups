## datatum 25/04/2018

library("ggplot2")
install.packages("devtools")
install.packages("latticeExtra")
install.packages("multcomp")
install.packages("gridExtra")
library(devtools)
library(latticeExtra)
library(multcomp)
library(labestData)
library(gridExtra)
install_git(url = "https://gitlab.c3sl.ufpr.br/pet-estatistica/labestData.git",
            branch = "master", build_vignettes = TRUE)

dados <- labestData::MingotiAnA3
names(dados)

ggplot(data = dados,
       aes(x = rendpc, y = rendpc,
           color = factor(inst))) +
  geom_point()

###

help(MingotiAnA3)

ggplot(data = dados,
       aes(x = factor(loc), y = rendm, color=factor(inst),
           fill = factor(loc), alpha = 0.3)) +
         geom_violin() +
         facet_grid(~factor(inst)) +
         theme_dark() +
         xlab("Localidade da Residência") +
         ylab("Renda Familiar Mensal") 
        
