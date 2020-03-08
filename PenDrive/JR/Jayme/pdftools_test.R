library(pdftools)
pdf_file <- "boleto_D978E940-88EA-4C9A-85A9-339A0F38696F.pdf"
info <- pdf_info(pdf_file)
text <- pdf_text(pdf_file)
fonts <- pdf_fonts(pdf_file)
files <- pdf_attachments(pdf_file)
info
text
fonts
files

pdf_render_page(pdf_file, page = 1, dpi = 100, numeric = FALSE)


pdf_toc(pdf_file)
pdftools::poppler_config()


if (!require("ghit")) {
    install.packages("ghit")
}

# elsewhere
ghit::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))

library(tabulizer)
install.packages("tabulizer")


library("stats4")

install.packages("stats4", dep = TRUE)
install.packages("sp", dep = TRUE)
install.packages("classInt", dep = TRUE)
install.packages("RColorBrewer", dep = TRUE)

require(sp) #pacote para analise de pontos
require(classInt) #pacote para criar intervalos de classe
require(RColorBrewer) #pacote para paleta de cores


n=100
xc = round(runif(n), 2)
yc = round(runif(n), 2)
xy = cbind(xc, yc)
xy

xy.sp = SpatialPoints(xy)
xy.sp

#Plotar pontos
plot(xy.sp, pch = 2)

##adicionar eixos
plot(xy.sp, pch = 2,axes = TRUE)

xy.cc = coordinates(xy.sp)
dim(xy.cc)
bbox(xy.sp)
dimensions(xy.sp)