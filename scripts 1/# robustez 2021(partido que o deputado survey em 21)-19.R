# robustez 2021(partido que o deputado survey 21)-19ADC

library(dplyr)
library(ggplot2)
library(gridExtra)
library(readr)
library(labelled)
rm(list=ls())

#primeiro arrumar povo 
library(haven)
povofull <- read_sav("Banco de Dados A Cara da Democracia 2019.sav")
povofull$partsimp2#partido - ver código aqui
povo <- povofull


povofull$temas7#aborto - 1  FAVOR 2 contra 3 depende e  88 e 99 NA)
povo$temas7[povo$temas7>4]<-NA# remover NA
table(povo$temas7, useNA="always")
#recodificar para valores entre 0 e 1 de modo a nao perder depende 
#e na ordem desejada com valor de fundamentalismo maior (1)

povo$contraaborto <- ifelse(povo$temas7 == 1, 0,
                            ifelse(povo$temas7 == 3, 0.5,
                                   ifelse(povo$temas7 == 2, 1, NA)))


table(povo$contraaborto)
#partido
povofull$partsimp2#partido 
table(povo$partsimp2)
#código-, partido-, quant- (mais de 20 ,depois ver se tem 3 na elite)- elite(+ de 3?)
#4 - MDB - 25 - deve ter
#27- PSL - 44 - deve ter
#30- PT - 292 - deve ter

# esses partidos devem ter na elite
#então já decidimos criar a variável com eles antes da base de elite
# e fechar a base toda antes de trabalhar com a base da elite

povo$partido <- NA
povo$partido[povo$partsimp2 == 4] <- "MDB"
povo$partido[povo$partsimp2 == 27] <- "PSL"
povo$partido[povo$partsimp2 == 30] <- "PT"
table(povo$partido, useNA = "always")
povo_aborto <- subset(povo,select=c(contraaborto,partido))
povo_abortof <- povo_aborto

povofull$econger#economia - 88 e 99 NA  e 10 prómercado (0 pro estado) - direçao OK
povo$econger[povo$econger>11]<-NA
table(povo$econger, useNA="always")
povo$proMercado <- povo$econger
povo <- remove_labels(povo)
povo$proMercado <- scales::rescale(povo$proMercado, to = c(0, 1))
povo_merca <- subset(povo, select=c(proMercado,partido))
summary(povo_merca)
summary(povo_aborto)
povo_mercaf <- povo_merca

#
#elite

#partido
load("C:/Users/grego/OneDrive/Desktop/work/Congruencia so brasil/BLS9_full.RData")
df <- bls[bls$wave == 2021, ]
dffull <- df
rm(bls)
table(df$wave)
df$party_survey#usar party survey no outro


df$partido <- NA
df$partido[df$party_survey==13]<- "PT"
df$partido[df$party_survey==172]<-"PSL"
df$partido[df$party_survey==15]<-"MDB"
table(df$partido, useNA= "always")

df$inicpriv
df$inicpriv[df$inicpriv == -999] <- NA
table(df$inicpriv, useNA = "always") #pro estado - inverter
df$inicpriv <- -1*df$inicpriv
df$inicpriv <- scales::rescale(df$inicpriv, to = c(0, 1))
table(df$inicpriv, useNA = "always") #pro mercado - OK
df$proMercado <- df$inicpriv

df_merca <- subset(df, select=c(proMercado, partido))
df_merca -> df_mercaf
summary(df_merca)

df$aborto
df$aborto[df$aborto == -999] <- NA
table(df$aborto, useNA = "always") ##pro fundamentalismo - OK
df$aborto <- scales::rescale(df$aborto, to = c(0, 1))
table(df$aborto, useNA = "always") #pro fundamentalismo - OK

# vou deixar no original 

df$contraaborto <- df$aborto

df_aborto<-subset(df, select=c(contraaborto,partido))
df_abortof <- df_aborto
#

#remoção de NAS:

complete5 <- function(...) {
  study <- df_mercaf %>%
    select(...)
  
  return(study[complete.cases(study),])
}

df_merca <- complete5(proMercado,partido)
summary(df_merca)

complete52 <- function(...) {
  study <- povo_mercaf %>%
    select(...)
  
  return(study[complete.cases(study),])
}

povo_merca <- complete52(proMercado,partido)
summary(povo_merca)

complete6 <- function(...) {
  study <- df_abortof %>%
    select(...)
  
  return(study[complete.cases(study),])
}

df_aborto <- complete6(contraaborto,partido)
summary(df_aborto)

complete62 <- function(...) {
  study <- povo_abortof %>%
    select(...)
  
  return(study[complete.cases(study),])
}

povo_aborto <- complete62(contraaborto,partido)
summary(povo_aborto)


# Lista dos objetos que você deseja manter
objetos_a_manter <- c("df_aborto", "df_abortof", "df_merca", "df_mercaf",
                      "povo_aborto", "povo_abortof", "povo_merca",
                      "povo_mercaf")

# Remove todos os objetos, exceto os objetos que você deseja manter
rm(list = setdiff(ls(), objetos_a_manter))


#
# aborto#
df_aborto$partido <- as.factor(df_aborto$partido)
povo_aborto$partido <- as.factor(povo_aborto$partido)


#
summary(df_aborto)
summary(povo_aborto)

#
#media
tapply(povo_aborto$contraaborto, povo_aborto$partido, mean)#povo
tapply(df_aborto$contraaborto, df_aborto$partido, mean)#elite
#

#macro gap contraaborto
mean_povo_aborto_contraaborto <- mean(povo_aborto$contraaborto)
mean_elite_contraaborto <- mean(df_aborto$contraaborto)
macroGap_contraaborto <- abs(mean_povo_aborto_contraaborto-mean_elite_contraaborto)
macroGap_contraaborto#

# macro de cada partido
partidos <- unique(df_aborto$partido)

macroGap_contraaborto <- sapply(partidos, function(partido) {
  mean_povo_aborto_contraaborto <- mean(povo_aborto$contraaborto[povo_aborto$partido == partido])
  mean_elite_contraaborto <- mean(df_aborto$contraaborto[df_aborto$partido == partido])
  abs(mean_povo_aborto_contraaborto - mean_elite_contraaborto)
})

result <- data.frame(partido = partidos, macroGap_contraaborto = macroGap_contraaborto)

#resultado macro gap por partido
print(result)#aqui!


#microGap contraaborto
povo_aborto$microGap_contraaborto <- abs(povo_aborto$contraaborto - mean_elite_contraaborto)
mean(povo_aborto$microGap_contraaborto)
# Calcular a média de povo_aborto$microGap_contraaborto por povo_aborto$partido
mean_microgap_by_partido_contraaborto <- tapply(povo_aborto$microGap_contraaborto, povo_aborto$partido, mean)

# Resultado
print(mean_microgap_by_partido_contraaborto)#aqui!

summary(povo_aborto$microGap_contraaborto)#descritivas

#

#proMercado


#
df_merca$partido <- as.factor(df_merca$partido)
povo_merca$partido <- as.factor(povo_merca$partido)

#
summary(df_merca)
summary(povo_merca)

#media
tapply(povo_merca$proMercado, povo_merca$partido, mean)#povo
tapply(df_merca$proMercado, df_merca$partido, mean)#elite
#

#macro gap proMercado
mean_povo_merca_proMercado <- mean(povo_merca$proMercado)
mean_elite_proMercado <- mean(df_merca$proMercado)
macroGap_proMercado <- abs(mean_povo_merca_proMercado-mean_elite_proMercado)
macroGap_proMercado#

# macro de cada partido
partidos <- unique(df_merca$partido)

macroGap_proMercado <- sapply(partidos, function(partido) {
  mean_povo_merca_proMercado <- mean(povo_merca$proMercado[povo_merca$partido == partido])
  mean_elite_proMercado <- mean(df_merca$proMercado[df_merca$partido == partido])
  abs(mean_povo_merca_proMercado - mean_elite_proMercado)
})

result <- data.frame(partido = partidos, macroGap_proMercado = macroGap_proMercado)

#resultado macro gap por partido
print(result)#aqui!


#microGap proMercado
povo_merca$microGap_proMercado <- abs(povo_merca$proMercado - mean_elite_proMercado)
mean(povo_merca$microGap_proMercado)
# Calcular a média de povo_merca$microGap_proMercado por povo_merca$partido
mean_microgap_by_partido_proMercado <- tapply(povo_merca$microGap_proMercado, povo_merca$partido, mean)

# Resultado
print(mean_microgap_by_partido_proMercado)#aqui!

summary(povo_merca$microGap_proMercado)#descritivas

#


#
#histogramas

df_merca$base <- "Elite" 
povo_merca$base <- "Povo"


merge_merca <- full_join(df_merca,povo_merca)

#proMercado povo_merca

proMercado_tab1 <- ggplot(povo_merca, aes(proMercado)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "Pró Mercado", title = "Público; distribuição")+theme_minimal()
proMercado_tab1#para testar
#proMercado elite
proMercado_tab2 <- ggplot(df_merca, aes(proMercado)) +
  geom_histogram(fill = "red", bins = 30) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "Pró Mercado", title = "Elite; distribuição")+theme_minimal()
proMercado_tab2#para testar

#microGap proMercado
proMercado_tab3 <- ggplot(povo_merca, aes(microGap_proMercado)) +
  geom_histogram(fill = "black", bins = 30) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "Micro Gap Pró Mercado", title = "Micro Gap; distribuição")+theme_minimal()
proMercado_tab3#para testar

# Gráfico de caixa de povo_merca$microGap_proMercado por categoria de povo_merca$partido
proMercado_tab4 <- ggplot(povo_merca, aes(x = partido, y = microGap_proMercado, fill = partido)) +
  geom_boxplot() +
  labs(x = "Partido", y = "microGap_proMercado") +
  scale_fill_discrete(name = "Partido") +
  ggtitle("Micro Gap Visão Pró Mercado por partido; distribuição")
proMercado_tab4#testar
# Gráfico de caixa de df_merca$proMercado por categoria de df_merca$partido
proMercado_tab5 <- ggplot(df_merca, aes(x = partido, y = proMercado, fill = partido)) +
  geom_boxplot() +
  labs(x = "Partido", y = "proMercado") +
  scale_fill_discrete(name = "Partido") +
  ggtitle("Posição das elites Visão Pró Mercado por partido; distribuição")
proMercado_tab5#testar


# Gráfico de caixa de povo_merca$proMercado por categoria de df_merca$partido
proMercado_tab6<- ggplot(povo_merca, aes(x = partido, y = proMercado, fill = partido)) +
  geom_boxplot() +
  labs(x = "Partido", y = "proMercado") +
  scale_fill_discrete(name = "Partido") +
  ggtitle("Posição do público Visão Pró Mercado por partido; distribuição")
proMercado_tab6#testar

# Definir as dimensões da área de plotagem
grid.arrange(proMercado_tab5,proMercado_tab2,proMercado_tab6,proMercado_tab1,proMercado_tab4,
             proMercado_tab3)

#merge_merca tabela 7
# Transformar a variável merge_merca$base em um fator
merge_merca$base_factor <- factor(merge_merca$base)

# Criar o gráfico com facet_wrap
proMercado_tab7 <- ggplot(merge_merca, aes(x = partido, y = proMercado, fill = partido)) +
  geom_boxplot() +
  labs(x = "Partido", y = "proMercado") +
  scale_fill_discrete(name = "Partido") +
  ggtitle("Visão Pró Mercado por partido; distribuição") +
  facet_wrap(~ base_factor)  # Adicionando o facet_wrap

proMercado_tab7


#
#

df_aborto$base <- "Elite" 
povo_aborto$base <- "Povo"


merge_aborto <- full_join(df_aborto,povo_aborto)

#contraaborto povo_aborto

contraaborto_tab1 <- ggplot(povo_aborto, aes(contraaborto)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "Posição contrária ao Aborto", title = "Público; distribuição")+theme_minimal()
contraaborto_tab1#para testar
#contraaborto elite
contraaborto_tab2 <- ggplot(df_aborto, aes(contraaborto)) +
  geom_histogram(fill = "red", bins = 30) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "Posição contrária ao Aborto", title = "Elite; distribuição")+theme_minimal()
contraaborto_tab2#para testar

#microGap contraaborto
contraaborto_tab3 <- ggplot(povo_aborto, aes(microGap_contraaborto)) +
  geom_histogram(fill = "black", bins = 30) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "Micro Gap Posição contrária ao Aborto", title = "Micro Gap; distribuição")+theme_minimal()
contraaborto_tab3#para testar

# Gráfico de caixa de povo_aborto$microGap_contraaborto por categoria de povo_aborto$partido
contraaborto_tab4 <- ggplot(povo_aborto, aes(x = partido, y = microGap_contraaborto, fill = partido)) +
  geom_boxplot() +
  labs(x = "Partido", y = "microGap_Posição contrária ao Aborto") +
  scale_fill_discrete(name = "Partido") +
  ggtitle("Micro Gap Posição contrária ao Aborto por partido; distribuição")
contraaborto_tab4#testar
# Gráfico de caixa de df_aborto$contraaborto por categoria de df_aborto$partido
contraaborto_tab5 <- ggplot(df_aborto, aes(x = partido, y = contraaborto, fill = partido)) +
  geom_boxplot() +
  labs(x = "Partido", y = "Posição contrária ao Aborto") +
  scale_fill_discrete(name = "Partido") +
  ggtitle("Posição das elites Posição contrária ao Aborto por partido; distribuição")
contraaborto_tab5#testar


# Gráfico de caixa de povo_aborto$contraaborto por categoria de df_aborto$partido
contraaborto_tab6<- ggplot(povo_aborto, aes(x = partido, y = contraaborto, fill = partido)) +
  geom_boxplot() +
  labs(x = "Partido", y = "Posição contrária ao Aborto") +
  scale_fill_discrete(name = "Partido") +
  ggtitle("Posição do público Posição contrária ao Aborto  por partido; distribuição")
contraaborto_tab6#testar

# Definir as dimensões da área de plotagem
grid.arrange(contraaborto_tab5,contraaborto_tab2,contraaborto_tab6,contraaborto_tab1,contraaborto_tab4,
             contraaborto_tab3)
#
#merge_aborto tabela 7
# Transformar a variável merge_aborto$base em um fator
merge_aborto$base_factor <- factor(merge_aborto$base)

# Criar o gráfico com facet_wrap
contraaborto_tab7 <- ggplot(merge_aborto, aes(x = partido, y = contraaborto, fill = partido)) +
  geom_boxplot() +
  labs(x = "Partido", y = "Posição contrária ao Aborto") +
  scale_fill_discrete(name = "Partido") +
  ggtitle("Posição contrária ao Aborto por partido; distribuição") +
  facet_wrap(~ base_factor)  # Adicionando o facet_wrap

contraaborto_tab7

povo_merca$microGap_proMercado -> povo_merca$microGap
povo_aborto$microGap_contraaborto -> povo_aborto$microGap
povo_merca$Dimensão <- "Pró Estado V.S Pró Mercado"
povo_aborto$Dimensão <- "Pró Aborto V.S. Contra Aborto"
merge <- full_join(povo_merca,povo_aborto)
summary(merge)
merge <- subset(merge, select=c(partido, microGap, Dimensão))
merge$Período <- "2021-2019"
summary(merge)
write.csv(merge,
          "C:/Users/grego/OneDrive/Desktop/work/Congruencia so brasil/graficos oscilacao no tempo/mer21_19.csv")
