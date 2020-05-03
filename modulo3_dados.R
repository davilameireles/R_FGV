#Modulo 3
#Carregamento e manipulacao de dados

#pacote: Tidyverse

##rocessar dados: dplyr, tidyr, stringr, lubridate;
##visualização de dados: ggplot2 e
##importar ou exportar dados: haven, httr, readxl, readr, rvest.

library(Zelig); library(tidyverse)
data(PErisk)




#banco_pos_selecao <- select(banco_inicial, var1, var2, var3, ...)
PErisk_so_quanti <- select(PErisk, barb2, gdpw2)



#selecionar os bancos de acordo com características dos nomes das variáveis
 names(PErisk) #ver nomes das variaveis
PErisk_so_com_co <- select(PErisk, starts_with('co'))

PErisk_termina_com_2 <- select(PErisk, ends_with('2'))

PErisk_contem_exp <- select(PErisk, contains('exp'))

PErisk_entre_vars <- select(PErisk, courts:prscorr2)

PErisk_menos_entre_vars <- select(PErisk, -(courts:prscorr2)) #não considera as vars mencionas 



# usar o select para selecionar e renomear variáveis:
PErisk_barb_renome <- select(PErisk, premio_mercado_clandestino = barb2)

PErisk_renome <- select(PErisk, vars = courts:prscorr2) # nomeando com vars1, vars2 ...



#Filter
PErisk_filtrado2 <- filter(PErisk, courts == 1, prscorr2 < 3)



#arrange
#ordenar o banco de dados
PErisk_ordenadoPIB <- arrange(PErisk, gdpw2)

PErisk_ordenadoPIB2 <- arrange(PErisk, desc(gdpw2))



#mutate e transmute
PErisk_PIBemDolares <- mutate(PErisk, PIBpc = exp(gdpw2))

PErisk_transmuted <- transmute(PErisk,
                               PIBpc = exp(gdpw2),
                               risco = 10-(prsexp2 + prscorr2))

PErisk_transmuted2 <- mutate(PErisk_transmuted,
                             riscoNota = ifelse(risco>5, 'Alto', 'Baixo'))
#diferença entre mutate e transmute é que mutate adiciona outra(s) 
#coluna(s) enquanto transmute mantém só as colunas que você usa.



#group_by + summarize
PErisk_summarized <- summarize(PErisk,
                               courts_media = mean(courts, na.rm=T),
                               barb2_media = mean(barb2, na.rm=T),
                               gdpw2_media = mean(gdpw2, na.rm=T))
#na.rm=T: excluir valores perdidos

PErisk_summarized2 <- summarize(PErisk,
                                courts_mediana = median(courts, na.rm=T,
                                barb2_mediana = median(barb2, na.rm=T),
                                gdpw2_mediana = median(gdpw2, na.rm=T))


#sample_*
sample <- sample_n(PErisk, 10) 

# podemos retirar amostras com reposiç
samples_replace <- sample_n(PErisk, 10, replace=T)
ão

#sorteio seja feito levando em conta as frequências dentro de cada grupo
PErisk_agrupado <- group_by(PErisk, courts)
smaple_agrupado <-sample_n(PErisk_agrupado, 5)
#seleciona cinco casos com judiciários independentes e cinco casos que não têm judiciários independentes

# amostrar frações do banco de dados usando a função sample_frac
samplefrac <- sample_frac(PErisk, 0.1)



#Tubos (sequência de comandos)
PErisk_piped <- PErisk %>% # Primeiro informamos o banco de dados
  filter(prscorr2 < 3) %>% # Passo 1: selecionar só os países com alta taxa de corrupção
  group_by(courts) %>% # Passo 2: agrupar os dados por presença e ausência de cortes
  summarize(barb2_media = mean(barb2, na.rm=T),
            gdpw2_media = mean(gdpw2, na.rm=T),
            gdpw2dol_media = exp(mean(gdpw2, na.rm=T))) # Passo 3: calcular as médias das variáveis e retornar os resultados

##courts barb2_media gdpw2_media gdpw2dol_media
##<int>       <dbl>       <dbl>          <dbl>
##  1      0       -1.58        8.40          4439.
##2      1       -1.96        8.92          7455.



#distinct
PErisk_dupl <- sample_frac(PErisk, 5, replace=T) %>%
  arrange(country) # Pede um sample com o cinco vezes o tamanho do banco de dados...
head(PErisk_dupl)

#excluir as observações repetidas do PErisk_dupl
PErisk_distinct <- PErisk_dupl %>% distinct()




dat <- data.frame(
  id = c(1,2,3),
  grupo = c('g1', 'g2', 'g3'),
  treatment_a = c(.36,.25,.3),
  treatment_b = c(.15,.12,.13),
  treatment_c = c(.43,.33,.35)
)

#pode ser escrita asssim...
dat_alt <- data.frame(
  id = c(1,1,1,2,2,2,3,3,3),
  grupo = c('g1', 'g1', 'g1', 'g2', 'g2', 'g2', 'g3', 'g3', 'g3'),
  treatment = c('a','b','c','a','b','c','a','b','c'),
  valor = c(.36,.15,.43,.25,.12,.33,.3,.13,.35)
)



#separate
df <- data.frame(x = c("a.b", "a.d", "b.c"))
# Separando em colunas A e B, usando o ponto como separador:
df %>% separate(x, c("A", "B"), sep='\\.')


df <- data.frame(x = c('a.b.c', 'a.d.f.h', 'b.c'))
# Separando em colunas A até D:
df %>% separate(x, c('A', 'B', 'C', 'D'), sep='\\.')
## Warning: Expected 4 pieces. Missing pieces filled with `NA` in 2 rows


df <- data.frame(x = c("a.b", "a.d", "b.c"))
# Separando em colunas A e B, usando o ponto como separador:
df_separate <- df %>% separate(x, c("A", "B"), sep='\\.')
df_separate
# Agora unindo de volta as variáveis:
df_separate %>% unite(x, A, B)


df <- data.frame(x = c('a.b.c', 'a.d.f.h', 'b.c'))
df
df_separate <- df %>%
  separate(x, c('A', 'B', 'C', 'D'), sep='\\.')
## Warning: Expected 4 pieces. Missing pieces filled with `NA` in 2 ro
df_separate %>% unite(x, A,B,C,D)


dat <- data.frame(
  caso = c('c1','c1','c1','c2','c2','c2','c3','c3','c3'),
  treat = c(1,2,3,1,2,3,1,2,3),
  result = c(.1,.2,.1,.5,.2,.9,1,.1,0)
)
#compactar esses dados...
dat_spread <- dat %>% spread(caso,result)
#gather reverte o que a função spread faz
dat_spread %>% gather(caso, result, -treat)
