# repetir e remover menção a partido

# 2014-2014
library(dplyr)
library(ggplot2)
library(gridExtra)
library(readr)
library(labelled)
rm(list=ls())
povo <- read_csv("df6.csv")
povo$E179WVS[is.na(povo$E179WVS)] <- -55
table(povo$E179WVS)# abaixo os com mais de 20 , falta ver se tem pelo menos 3 deputados
#partidos WVS
#76001	BRA:Brazilian Democratic Movement = + q 20
#76002	BRA:Workers´ Party = + q 20
#76003	BRA:Brazilian Social Democracy Party = + q 20
#76004	BRA:Democratic Labour Party = + q 20
#76012  BRA: Brazilian Socialist Party = + q 20

library(haven)
dffull <-read_sav("BASEDATOS_BRASIL_102.sav")
df <- remove_labels(dffull)
print(dffull$partido)
table(df$partido)#abaixo mais q três E QUE TEM NA LISTA DO POVO
#493                  PDT - 5 DEPUTADOS OK
#494                   PT - 22 DEPUTADOS OK
#496                 PMDB - 15 DEPUTADOS OK
#501                  PSB - 6 DEPUTADOS OK
#503                 PSDB - 22 DEPUTADOS OK


df$partido -> df$partido_formatoriginal
#publico
povo$partido <- NA
povo$partido[povo$E179WVS == 76001] <- "PMDB"
povo$partido[povo$E179WVS == 76002] <- "PT"
povo$partido[povo$E179WVS == 76003] <- "PSDB"
povo$partido[povo$E179WVS == 76004] <- "PDT"
povo$partido[povo$E179WVS == 76012] <- "PSB"
table(povo$partido, useNA = "always")
#elite
df$partido <- NA
# Substituindo valores específicos de acordo com a tabela fornecida
df$partido[df$partido_formatoriginal == 496] <- "PMDB"
df$partido[df$partido_formatoriginal == 494] <- "PT"
df$partido[df$partido_formatoriginal == 503] <- "PSDB"
df$partido[df$partido_formatoriginal == 493] <- "PDT"
df$partido[df$partido_formatoriginal == 501] <- "PSB"
table(df$partido, useNA="always")

table(povo$E036)
# Recodificação recodifiquei 1 e 2 como 1; 3 e 4 como 2; 5 e 6 como 3; 7 e 8 como 4; e 9 e 10 como 5
str(povo$E036)
povo$E036 <- cut(povo$E036, breaks = c(0, 2, 4, 6, 8, 10), labels = c(1, 2, 3, 4, 5), include.lowest = TRUE)
table(povo$E036)
povo$E036 <- as.numeric(povo$E036)
table(povo$E036, useNA = "always")
summary(povo$E036)
-1*povo$E036 -> povo$proMercado
print(dffull$EM1)#10 mercado
table(df$EM1, useNA = "always")# Um na arrumar depois
df$proMercado <- df$EM1
df$proMercado <- scales::rescale(df$proMercado, to = c(0, 1))
povo$proMercado <- scales::rescale(povo$proMercado, to = c(0, 1))
table(df$proMercado, useNA = "always")
table(povo$proMercado, useNA = "always")#arrumar na depois
summary(df$proMercado)
summary(povo$proMercado)

#
#fundamentalismo, as que tem
print(dffull$VAL2)#cas gay
table(df$VAL2)#10 é favor - inverter
print(dffull$VAL6)#aborto
table(df$VAL6)# 10 é favor - inverter

#inverter no scores

df <- subset(df, select=c(VAL2,VAL6,proMercado))

df->dffull
complete5 <- function(...) {
  study <- dffull %>%
    select(...)
  
  return(study[complete.cases(study),])
}

df <- complete5(VAL2,VAL6,proMercado)#como nao tem NA no proMercado
summary(df)

library(psych)
library(semTools)
library(lavaan)
scree(df[,1:3])
pcaelite <-psych::principal(df[,1:3],2)
pcaelite#PRA TESTAR

model <- '
  # Especificação do modelo fatorial
  LiberalFundamentalismo =~ VAL2+VAL6
  F2 =~ proMercado
 
  # Especificação das variâncias dos erros
  VAL2 ~~ VAL2
  VAL6 ~~ VAL6
  proMercado ~~ proMercado


'
# Ajuste do modelo de CFA
fit <- cfa(model, data = df)

summary(fit)
semTools::fitmeasures(fit, c("tli", "cfi", "rmsea", "srmr"))#
scores <- lavPredict(fit)
-1*scores[,1] -> df$fundamentalismo#inverter
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
povo <- subset(povo, select=c(F118,F120,F028,F121,proMercado,partido))
summary(povo)
table(povo$F118)
povo$F118[povo$F118<0]<-NA
povo$F120[povo$F120<0]<-NA
povo$F121[povo$F121<0]<-NA
povo$F028[povo$F028<0]<-NA



#ver sentido das questões
#Obs no arquivo criar base
#sentidos originais das questões
#print(df7$F118)#liberal (valores positivos)CasGay
#print(df7$F120)#liberal (valores positivos)Aborto
#print(df7$F028)#valores positivos pouca religiosidade
#print(df7$F121)#valores positivos liberal (divorce)
#todos os valores positivos são liberal, basta inverter no score
#inverter no score(lembrete 1)


povofull <- povo
summary(povofull)

complete52 <- function(...) {
  study <- povofull %>%
    select(...)
  
  return(study[complete.cases(study),])
}

povo <- complete52(F118,F120,F028,F121,proMercado)#como nao tem NA no proMercado
summary(povo)


scree(povo[,1:5])

pcapovo <-psych::principal(povo[,1:5],2)

pcapovo

model <- '
  # Especificação do modelo fatorial
  LiberalFundamentalismo =~ F118+F120+F028+F121

  # Especificação das variâncias dos erros
  F118 ~~ F118
  F121 ~~ F121
  F028 ~~ F028
  F120 ~~ F120

'
# Ajuste do modelo de CFA
fit <- cfa(model, data = povo)

summary(fit)
semTools::fitmeasures(fit, c("tli", "cfi", "rmsea", "srmr"))#
scores <- lavPredict(fit)
povo$fundamentalismo <- -1*scores[,1]#inverter (lembrete 1)
#

rm(model,scores,fit)
#table(povo$partido)
povo <- subset(povo, select=c(proMercado,fundamentalismo))
#povo$partido <- as.factor(povo$partido)
povo$fundamentalismo <- scales::rescale(povo$fundamentalismo, to = c(0, 1))

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



