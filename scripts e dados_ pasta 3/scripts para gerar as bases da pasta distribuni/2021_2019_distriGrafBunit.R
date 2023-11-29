#2021_BLS_2019_ADC

#
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

povo <- complete52(freqigreja_invertido,temas7,temas2,econger,partido)#como nao tem NA no proMercado
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
table(povo$partido)
povo <- subset(povo, select=c(partido,econger,fundamentalismo))
povo$partido <- as.factor(povo$partido)
povo$fundamentalismo <- scales::rescale(povo$fundamentalismo, to = c(0, 1))
povo$proMercado <- povo$econger
povo$proMercado <- scales::rescale(povo$proMercado, to = c(0, 1))
povo <- subset(povo, select=c(partido,proMercado,fundamentalismo))
summary(povo)

#agora sim   base elite BLS 2021
#elite

#partido
load("C:/Users/grego/OneDrive/Desktop/work/Congruencia so brasil/BLS9_full.RData")
df <- bls[bls$wave == 2021, ]
dffull <- df
rm(bls)
table(df$wave)
df$party_survey
table(df$party_survey)#consultar codebook!
#PT código 13 - tem dezessete deputados
#PSL código 172 - tem dez deputados
#MDB código 15 tem 9 deputado
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

df <- complete5(aborto,valcrist,casament_invertido,proMercado,partido)#como nao tem NA no proMercado
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
table(df$partido)
df <- subset(df, select=c(partido,proMercado,fundamentalismo))
df$partido <- as.factor(df$partido)
df$fundamentalismo <- scales::rescale(df$fundamentalismo, to = c(0, 1))
summary(df)
#


#descritivas
pcapovo
pcaelite
#povo
summary(povo)


#   
#elite
summary(df)


#comparador medias e medianas
#media
tapply(povo$proMercado, povo$partido, mean)#povo
tapply(df$proMercado, df$partido, mean)#elite
#mediana
tapply(povo$proMercado, povo$partido, median)#povo
tapply(df$proMercado, df$partido, median)#elite

#media
tapply(povo$fundamentalismo, povo$partido, mean)#povo
tapply(df$fundamentalismo, df$partido, mean)#elite
#mediana
tapply(povo$fundamentalismo, povo$partido, median)#povo
tapply(df$fundamentalismo, df$partido, median)#elite

#microgap



#
#macro gap proMercado
mean_povo_proMercado <- mean(povo$proMercado)
mean_elite_proMercado <- mean(df$proMercado)
macroGap_proMercado <- abs(mean_povo_proMercado-mean_elite_proMercado)
macroGap_proMercado#

# macro de cada partido
partidos <- unique(df$partido)

macroGap_proMercado <- sapply(partidos, function(partido) {
  mean_povo_proMercado <- mean(povo$proMercado[povo$partido == partido])
  mean_elite_proMercado <- mean(df$proMercado[df$partido == partido])
  abs(mean_povo_proMercado - mean_elite_proMercado)
})

result <- data.frame(partido = partidos, macroGap_proMercado = macroGap_proMercado)


# Resultado
print(result)#aqui!

#microGap proMercado
povo$microGap_proMercado <- abs(povo$proMercado - mean_elite_proMercado)
mean(povo$microGap_proMercado)
# Calcular a média de povo$microGap_proMercado por povo$partido
mean_microgap_by_partido_proMercado <- tapply(povo$microGap_proMercado, povo$partido, mean)

# Resultado
print(mean_microgap_by_partido_proMercado)#aqui!

summary(povo$microGap_proMercado)#descritivas

#macro gap fundamentalismo
mean_povo_fundamentalismo <- mean(povo$fundamentalismo)
mean_elite_fundamentalismo <- mean(df$fundamentalismo)
macroGap_fundamentalismo <- abs(mean_povo_fundamentalismo-mean_elite_fundamentalismo)
macroGap_fundamentalismo#

# macro de cada partido
partidos <- unique(df$partido)

#
macroGap_fundamentalismo <- sapply(partidos, function(partido) {
  mean_povo_fundamentalismo <- mean(povo$fundamentalismo[povo$partido == partido])
  mean_elite_fundamentalismo <- mean(df$fundamentalismo[df$partido == partido])
  abs(mean_povo_fundamentalismo - mean_elite_fundamentalismo)
})

result <- data.frame(partido = partidos, macroGap_fundamentalismo = macroGap_fundamentalismo)

#resultado macro gap por partido
print(result)#aqui!


#microGap fundamentalismo
povo$microGap_fundamentalismo <- abs(povo$fundamentalismo - mean_elite_fundamentalismo)
mean(povo$microGap_fundamentalismo)
# Calcular a média de povo$microGap_fundamentalismo por povo$partido
mean_microgap_by_partido_fundamentalismo <- tapply(povo$microGap_fundamentalismo, povo$partido, mean)

# Resultado
print(mean_microgap_by_partido_fundamentalismo)#aqui!

summary(povo$microGap_fundamentalismo)#descritivas

#
options(scipen = 999)
# Realizar o teste t para a variável "fundamentalismo"
t_test_fundamentalismo <- t.test(df$fundamentalismo, povo$fundamentalismo)

# Realizar o teste t para a variável "proMercado"
t_test_proMercado <- t.test(df$proMercado, povo$proMercado)

# Exibir os resultados
print(t_test_fundamentalismo)
print(t_test_proMercado)
povo$base <- "Público"
df$base <- "Elite"
merge <- full_join(povo,df)
merge$ano <- "2021-2019"
write.csv(merge,
          "C:/Users/grego/OneDrive/Desktop/work/Congruencia so brasil/distri geral/distribuni/df_t5.csv")
