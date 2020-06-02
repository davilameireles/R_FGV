library(readxl)
setwd("C:/Users/davil/Desktop/Projeto R/Inflação_data.worldbank")
# set("rFGVmaterials-master/arquivos/") 
inflacao <- read_excel("Inflacao_data_worldbank.xls")
desemprego_modeloIOL <- read_excel("ModeloIOL_desemprego_data_worldbank.xls")
View(inflacao)
View(desemprego_modeloIOL)



#inflacao Brasil 1991 a 2005
inflacao_BR <- inflacao[29,36:64]
View(inflacoa_BR)
inflacao_BR

desemprego_BR <- desemprego_modeloIOL[29,36:64]
desemprego_BR


#modelo <-lm(inflacao_BR ~ desemprego_BR)

#vetores
x <- c()
y <- c() 
x[1]
x[1] <- 1
x 
y

for (i in 1:length(desemprego_BR)){
  y[i] <- desemprego_BR[[i]]
}

for (i in 1:length(inflacao_BR)){
  x[i] <- inflacao_BR[[i]]
}

regressao <- lm(x~y)
regressao

plot(x,y)
