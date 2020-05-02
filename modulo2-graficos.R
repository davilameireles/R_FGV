#Unidade 2 
#Gráficos no R:
#variáveis individuais

library(tidyverse)

#variaveis quantitativas 

#USArrests
data("USArrests")
head(USArrests)
tail(USArrests)
USArrests$Murder

sort(USArrests$Murder) # ordena
summary(USArrests) # Um sumário das variáveis
names(USArrests) # O nome das variáveis
row.names(USArrests) # Nomes dos casos/Estados 


#gráficos 
# histograma
  ggplot(data = USArrests)+
  geom_histogram(mapping = aes(x = Murder), binwidth = 1) # pode tirar esse binwidth, foi sugestão do proprio R


#Grafico de pontos (pontos com tamnaho 1)
 ggplot()+
  geom_dotplot(data = USArrests,
               mapping = aes(x = Murder),
               binwidth = 1, bins = 10)

#Densidade usando kernel gaussiano
#a ideia é suavizar os 'pulos' do histograma
ggplot(data = USArrests)+
  geom_density(mapping = aes(x = Murder),
               kernel = "gaussian")

#Boxplot
ggplot(data = USArrests)+
  geom_boxplot(mapping = aes(x = 1, y = Murder))

#violin-plot
ggplot(data = USArrests)+
  geom_violin(mapping = aes(x = 1, y = Murder)

#histograma mais completo
ggplot(data = USArrests)+
  geom_histogram(mapping = aes(x = Assault),
                 bins = 10)+                    
  labs( x = "Assaltos para cada 100 mil habitantes",
        y = "Frequencia",
        title = "Assaltos em estados americanos")
# bins reduz o número de divisões
# labs (x, y, title, subtitle, caption)






#variaveis qualitativas


install.packages("Zelig")
library(tidyverse)
data(PErisk)

ggplot(data = PErisk) +
  geom_bar(mapping = aes(x = courts))


ggplot(data = PErisk) +
  geom_bar(mapping = aes(x = factor(prsexp2)))
# factor, construímos fatores com os valores


ggplot(data = PErisk) +
  geom_bar(mapping = aes(x = factor(prsexp2)), fill = rainbow(6))
#rainbow gera cores do arco-íris
#o fill significa o preenchimento das colunas


#compara prscorr2 com courts
ggplot(data = PErisk) +
  geom_bar(mapping = aes(x = factor(prscorr2),
                         fill = factor(courts)),
           position = 'fill')


#transformar a variável de corrupção em binária
PErisk$lowcorrup = as.numeric(PErisk$prscorr2>2)
ggplot(data = PErisk) +
  geom_bar(mapping = aes(x = factor(lowcorrup),
                         fill = factor(courts)),
           position = 'fill')


#mostra, claramente, a relação entre independência de
judiciários e nível de corrupção
PErisk$lowcorrup_fator = factor(PErisk$lowcorrup)
levels(PErisk$lowcorrup_fator) <- c('Alta', 'Baixa')
PErisk$courts_fator = factor(PErisk$courts)
levels(PErisk$courts_fator) <- c('Não Independente', 'Independente')
ggplot(data = PErisk) +
  geom_bar(mapping = aes(x = lowcorrup_fator,
                         fill = courts_fator),
           position = 'fill') +
  labs(x = 'Corrupção', y = 'Proporção', fill = 'Status das Cortes')


#lowcorrup_fator - capta o nível alto ou baixo de corrupção em uma sociedade
#barb2 - ganho em logaritmo do mercado clandestino nos países
#para entendermos a dispersão ....

# Usando box-plot
ggplot(data = PErisk) +
  geom_boxplot(mapping = aes(x = lowcorrup_fator, y = barb2))

# Usando violin-plot
ggplot(data = PErisk) +
  geom_violin(mapping = aes(x = lowcorrup_fator, y = barb2))



#Quanti-quanti: o diagrama de dispersão
ggplot(data = PErisk) +
  geom_point(mapping = aes(x = barb2, y = gdpw2))

#podemos colocar uma reta ajustada
ggplot(data = PErisk) +
  geom_point(mapping = aes(x = barb2, y = gdpw2)) +
  geom_smooth(mapping = aes(x = barb2, y = gdpw2),
              method = 'lm')
#(method = 'lm')-  queremos uma reta ajustada por regressão linear simples 

#diferenciar os países de acordo com possuir ou não um judiciário independente
ggplot(data = PErisk) +
  geom_point(mapping = aes(x = barb2, y = gdpw2,
                           color = courts_fator)) +
  geom_smooth(mapping = aes(x = barb2, y = gdpw2),
              method = 'lm')

# Gráfico com uma só reta ajustada, mas diferenciando pontos
ggplot(data = PErisk) +
  geom_point(mapping = aes(x = barb2, y = gdpw2,
                           color = courts_fator)) +
  geom_smooth(mapping = aes(x = barb2, y = gdpw2),
              method = 'lm')

# Gráfico com duas retas ajustadas, para cada um dos tipos de pontos
ggplot(data = PErisk) +
  geom_point(mapping = aes(x = barb2, y = gdpw2,
                           color = courts_fator)) +
  geom_smooth(mapping = aes(x = barb2, y = gdpw2,
                            color = courts_fator),
              method = 'lm')


