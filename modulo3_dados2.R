#dir("rFGVmaterials-master/arquivos/")

library(readxl) #le excel
library(haven)
library(tidyverse)

setwd("~/R_FGV/rFGVmaterials-master/arquivos")
# set("rFGVmaterials-master/arquivos/") 

#Carregando arquivo de texto
banco_de_dados <- read_table('PErisk.dat')
#write.table(PErisk,"Teste_PErisk.dat")

#Carregando arquivo .csv
arqcsv <- read.csv("PErisk.csv")
summary(arqcsv)
help("read.csv")

#arquivo excel
#ussa pacote readxl
dat <- read_excel('plan1.xlsx', sheet=1)
head(dat)

#RData
load("PErisk.RData")
#save(PErisk, file ="Teste_PErisk.RData")
summary(PErisk)

#Arquivo SPSS
#Usa pacote haven
require(haven)
arqspss <- read_spss("PErisk.sav")
#write_sav("name.sav")



arqstata <- read_stata("PErisk.dta")
#write_dta("name.dta")


## Use setwd antes de abrir e salvar os arquivos 