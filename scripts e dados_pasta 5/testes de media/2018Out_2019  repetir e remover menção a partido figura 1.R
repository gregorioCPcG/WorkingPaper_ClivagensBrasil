#repetir e remover menção a partido figura 1

#2021_BLS _(partido eleito em 2018)_2019_ADC

#
library(dplyr)
library(ggplot2)
library(gridExtra)
library(readr)
library(labelled)
rm(list=ls())


#primeiro arrumar povo # repete 2021_BLS_2019_ADC
library(haven)
povofull <- read_sav("Banco de Dados A Cara da Democracia 2019.sav")
povofull$partsimp2#partido - ver código aqui
povo <- povofull

povofull$econger#economia - 88 e 99 NA  e 10 prómercado (0 pro estado) - direçao OK
povo$econger[povo$econger>11]<-NA
table(povo$econger, useNA="always")

povofull$temas2#cas gay - 2 contra (3 depende 88 e 99 NA ) - direçao OK
povo$temas2[povo$temas2>2]<-NA
table(povo$temas2, useNA="always")

povofull$temas7#aborto - 2 contra (3 depende 88 e 99 NA ) - direçao OK
povo$temas7[povo$temas7>2]<-NA
table(povo$temas7, useNA="always")

povofull$freqigreja# 88 e 99 NA - 6 nunca - inverter!!
povo$freqigreja[povo$freqigreja>80]<-NA
table(povo$freqigreja, useNA="always")

povo$freqigreja_invertido <- -1*povo$freqigreja
povo$freqigreja_invertido  <- scales::rescale(povo$freqigreja_invertido, to = c(0, 1))

povo<-subset(povo, select=c(partsimp2,freqigreja_invertido,temas7,temas2,econger))


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

povo<-subset(povo, select=c(freqigreja_invertido,temas7,temas2,econger,partido))
povofull <- povo
summary(povofull)

complete52 <- function(...) {
  study <- povofull %>%
    select(...)
  
  return(study[complete.cases(study),])
}

povo <- complete52(freqigreja_invertido,temas7,temas2,econger)#como nao tem NA no proMercado
summary(povo)
povo <- remove_labels(povo)
library(psych)
library(lavaan)
library(semTools)
scree(povo[,1:4])

pcapovo <-psych::principal(povo[,1:4],2)

pcapovo

model <- '
  # Especificação do modelo fatorial
  LiberalFundamentalismo =~ temas7+temas2+freqigreja_invertido

  # Especificação das variâncias dos erros
  temas7 ~~ temas7
  freqigreja_invertido ~~ freqigreja_invertido
  temas2 ~~ temas2

'
# Ajuste do modelo de CFA
fit <- cfa(model, data = povo)

summary(fit)
semTools::fitmeasures(fit, c("tli", "cfi", "rmsea", "srmr"))#
scores <- lavPredict(fit)
povo$fundamentalismo <- scores[,1]#
#

rm(model,scores,fit)
#table(povo$partido)
povo <- subset(povo, select=c(econger,fundamentalismo))
#povo$partido <- as.factor(povo$partido)
povo$fundamentalismo <- scales::rescale(povo$fundamentalismo, to = c(0, 1))
povo$proMercado <- povo$econger
povo$proMercado <- scales::rescale(povo$proMercado, to = c(0, 1))
povo <- subset(povo, select=c(proMercado,fundamentalismo))
summary(povo)

#agora sim   base elite BLS 2021
#elite

#partido
load("C:/Users/grego/OneDrive/Desktop/work/Congruencia so brasil/BLS9_full.RData")
df <- bls[bls$wave == 2021, ]
dffull <- df
rm(bls)
table(df$wave)
df$party_elected#aqui muda
table(df$party_elected)#consultar codebook!
table(df$party_survey)#para comparar


# Filtrar os dados para incluir apenas os casos desejados nos valores de party_survey e party_elected
filtered_df <- df[df$party_survey %in% c(172, 13, 15) | df$party_elected %in% c(172, 13, 15), ]

# Criar a tabela de contagem cruzada
cross_table <- table(filtered_df$party_survey, filtered_df$party_elected)

# Exibir a tabela
print(cross_table)



#PT código 13 - tem dezessete deputados
#PSL código 172 - tem dez deputados
#MDB código 15 tem 9 deputado
df$partido <- NA
df$partido[df$party_elected==13]<- "PT"
df$partido[df$party_elected==172]<-"PSL"
df$partido[df$party_elected==15]<-"MDB"
table(df$partido, useNA= "always")

df$inicpriv
df$inicpriv[df$inicpriv == -999] <- NA
table(df$inicpriv, useNA = "always") #pro estado - inverter
df$inicpriv <- -1*df$inicpriv
df$inicpriv <- scales::rescale(df$inicpriv, to = c(0, 1))
table(df$inicpriv, useNA = "always") #pro mercado - OK
df$inicpriv_invertido <- df$inicpriv

df$casament
df$casament[df$casament == -999] <- NA
table(df$casament, useNA = "always") #pro liberal - inverter
df$casament <- -1*df$casament
df$casament <- scales::rescale(df$casament, to = c(0, 1))
table(df$casament, useNA = "always") #pro fundamentalismo - OK
df$casament_invertido <- df$casament


df$aborto
df$aborto[df$aborto == -999] <- NA
table(df$aborto, useNA = "always") ##pro fundamentalismo - OK
df$aborto <- scales::rescale(df$aborto, to = c(0, 1))
table(df$aborto, useNA = "always") #pro fundamentalismo - OK


df$valcrist
df$valcrist[df$valcrist == -999] <- NA
table(df$valcrist, useNA = "always") ##pro fundamentalismo - OK
df$valcrist <- scales::rescale(df$valcrist, to = c(0, 1))
table(df$valcrist, useNA = "always") ##pro fundamentalismo - OK

df<- subset(df,select=c(aborto,valcrist,casament_invertido,inicpriv_invertido,partido))
summary(df)
df$proMercado <- df$inicpriv_invertido
dffull <- df

complete5 <- function(...) {
  study <- dffull %>%
    select(...)
  
  return(study[complete.cases(study),])
}

df <- complete5(aborto,valcrist,casament_invertido,proMercado)#como nao tem NA no proMercado
summary(df)

library(psych)
library(semTools)
library(lavaan)
scree(df[,1:4])
pcaelite <-psych::principal(df[,1:4],2)
pcaelite#PRA TESTAR

model <- '
  # Especificação do modelo fatorial
  LiberalFundamentalismo =~ aborto+casament_invertido+valcrist
 
  # Especificação das variâncias dos erros
  casament_invertido ~~ casament_invertido
  aborto ~~ aborto
  valcrist ~~ valcrist


'
# Ajuste do modelo de CFA
fit <- cfa(model, data = df)

summary(fit)
semTools::fitmeasures(fit, c("tli", "cfi", "rmsea", "srmr"))#
scores <- lavPredict(fit)
scores[,1] -> df$fundamentalismo
hist(df$fundamentalismo)
summary(df)
#
rm(model,scores,fit)
#table(df$partido)
df <- subset(df, select=c(proMercado,fundamentalismo))
#df$partido <- as.factor(df$partido)
df$fundamentalismo <- scales::rescale(df$fundamentalismo, to = c(0, 1))
summary(df)
#

summary(povo)
summary(df)
#
options(scipen = 999)
# Realizar o teste t para a variável "fundamentalismo"
t_test_fundamentalismo <- t.test(df$fundamentalismo, povo$fundamentalismo)

# Realizar o teste t para a variável "proMercado"
t_test_proMercado <- t.test(df$proMercado, povo$proMercado)

# Exibir os resultados
print(t_test_fundamentalismo)
print(t_test_proMercado)




