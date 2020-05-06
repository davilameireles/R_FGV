#Aula 30/03
install.packages("qqplot2")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("ggthemes")

#chamar biblioteca
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(plyr)  #
library(dplyr) #

estado = rep(c("RJ", "SP"), each = 20)
rec = rep(c("grupo a", "grupo b"), each = 10)
dose <- rep(seq(5,50,5),2)
custo <- c(seq(40,50,5), seq(53,56), seq(55,53),
           seq(45,55,5), seq(56,59), seq(58,56))
dados <- data.frame(estado,rec,dose,custo = c(custo,custo * 1.12))

#grafico de dispersao 
#scatterplot
GGPLOT_1 <- 
  ggplot() +
  geom_point(data = dados,
             aes(x = dose, y = custo))
GGPLOT_1



GGPLOT_2 <- 
  ggplot() +
  geom_point(data = dados,
             aes(x=dose, y = custo, colour = rec))
GGPLOT_2



GGPLOT_3 <-
  ggplot()+
  geom_point(data = dados,
             aes(x = dose, y = custo, colour = rec)) + # rotulo x
  xlab(expression(paste("Dose"-(comprimidos))))+ #rotulo y
  ylab(expression(paste("Custo"-(Reais))))+
  scale_x_continuous(breaks = seq(0,100.5),limits = c(0,55))+ #escala
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0,70))+ #escala
  theme(legend.position = "bottom")+   # posição da legenda
  theme(text = element_text(size=10)) # tamanho da fonte

GGPLOT_3


GGPLOT_4 <-
  ggplot()+
  geom_point(data = dados,
             aes(x = dose, y = custo, colour = rec)) + # rotulo x
  xlab(expression(paste("Dose"-(comprimidos))))+ #rotulo y
  ylab(expression(paste("Custo"-(Reais))))+
  scale_x_continuous(breaks = seq(0,100.5),limits = c(0,55))+ #escala
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0,70))+ #escala
  theme_bw() +
  theme(legend.position = "bottom")+   # posição da legenda
  theme(text = element_text(size=10)) # tamanho da fonte

GGPLOT_4



GGPLOT_RJ <-
  ggplot()+
  geom_point(data = subset(dados,estado="RJ"),
             aes(x = dose, y = custo, colour = rec)) + # rotulo x
  xlab(expression(paste("Dose"-(comprimidos))))+ #rotulo y
  ylab(expression(paste("Custo"-(Reais))))+
  scale_x_continuous(breaks = seq(0,100.5),limits = c(0,55))+ #escala
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0,70))+ #escala
  theme(legend.position = "bottom")+   # posição da legenda
  theme(text = element_text(size=10)) # tamanho da fonte

GGPLOT_RJ



GGPLOT_SP <-
  ggplot()+
  geom_point(data = subset(dados,estado="SP"),
             aes(x = dose, y = custo, colour = rec)) + # rotulo x
  xlab(expression(paste("Dose"-(comprimidos))))+ #rotulo y
  ylab(expression(paste("Custo"-(Reais))))+
  scale_x_continuous(breaks = seq(0,100.5),limits = c(0,55))+ #escala
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0,70))+ #escala
  theme(legend.position = "bottom")+   # posição da legenda
  theme(text = element_text(size=10)) # tamanho da fonte

GGPLOT_SP

GGPLOT_4a <- grid.arrange(GGPLOT_RJ, GGPLOT_SP, ncol = 2)



GGPLOT_4facet <- 
  ggplot()+
  geom_point(data=dados,
             aes(x = dose, y = custo, colour = rec))+
  xlab(expression(paste("Dose"-(comprimidos))))+
  ylab(expression(paste("Custo"-(Reais))))+
  scale_x_continuous(breaks = seq(0,100.5),limits = c(0,55))+ #escala
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0,70))+ #escala
  theme(legend.position = "bottom")+   # posição da legenda
  theme(text = element_text(size=10))+
  facet_grid(. ~estado)# tamanho da fonte

GGPLOT_4facet



GGPLOT_5<- 
  ggplot()+
  geom_bar(data = subset(dados,estado=="SP"),
           aes(x = dose, y = custo, fill = rec),
           stat = "identity") +
  xlab(expression(paste("Dose"-(comprimidos))))+
  ylab(expression(paste("Custo"-(Reais))))+
  scale_x_continuous(breaks = seq(0,100.5),limits = c(0,55))+ #escala
  theme_bw() +
  theme(legend.position = "bottom")+   # posição da legenda
  theme(text = element_text(size=10))  # tamanho da fonte

GGPLOT_5


GGPLOT_6 <- 
  ggplot() +
  geom_bar(data = subset(dados, estado == "RJ"),
           aes( x = dose, y = custo, fill = rec),
           stat = "identity",
           position = "dodge")+
  xlab(expression(paste("Dose"~(comprimidos))))+
  ylab(expression(paste("Custo"~(Reais))))+
  scale_x_continuous(breaks = seq(0,100,5), limits = c(0,55))+
  theme_bw()+
  theme(legend.position = 'botton')+
  theme(text = element_text(size = 10))
GGPLOT_6


GGPLOT_7 <- 
  ggplot() +
  geom_point(data =subset(dados, estado == "RJ"),
             aes(x=dose, y = custo, fill = rec))+
  geom_smooth(data = subset(dados, estado = "RJ"), # adiciona um ajuste polinomial 
              aes(x=dose, y = custo, fill = rec),
              method = lm,
              formula = y ~poly(x, 2, raw = TRUE))+
  xlab(expression(paste("Dose"~(comprimidos))))+
  ylab(expression("Custo"~(Reais)))+
  scale_x_continuous(breaks = seq(0, 100, 5), limits = c(0,55))+
  theme_bw()+
  theme(legend.position = "botton")+
  theme(text = element_text(size = 10))
GGPLOT_7




#funçoes
LM <- dlply(dados, c("estado", "rec"),
            function(df)
              (lm(custo ~poly(dose, 2, raw = TRUE), data = df)))

LM_DF <- ldply(LM, function(x) {
  r.sq <-summary(x)$r.squared
  intercept <- summary(x)$coefficients[1]
  beta <- summary(x)$coefficients[2]
  beta1 <- summary(x)$coefficients[3]
  data.frame(r.sq, intercept, beta, beta1)})

#sumario
#importante
knitr:: kable(LM_DF, format = 'markdown')



GGPLOT_FINAL <-
  ggplot() +
  geom_point(data = dados,
             aes(x = dose, y = custo, colour = rec))+
  facet_grid(rec ~estado)+
  geom_smooth(data = dados, 
              aes(x = dose, y = custo, colour = rec),
              method = lm,
              formula = y ~poly(x, 2, raw = TRUE))+
  geom_text(data = LM_DF, 
            aes(x = 25,
                y = 45, 
                label = paste("R^2==", LM_DF$r.sq, sep = "")),
            size = 2.8,
            parse = TRUE)+
  xlab(expression(paste("Dose"~(comprimidos))))+
  ylab(expression(paste("Custo"~(Reais))))+
  scale_x_continuous(breaks = seq(0,100,5), limits = c(0,55))+
  theme_bw()+
  theme(legend.position = "bottom")+
  guides(colour= FALSE)
GGPLOT_FINAL



