# 2014-2014 family
library(dplyr)
library(ggplot2)
library(gridExtra)
library(readr)
library(labelled)
library(psych)
library(lavaan)
library(semPlot)



# OBS ver esquema classificatório em familys e classificacao adotada.docx


#

rm(list=ls())
povo <- read_csv("df6.csv")
povo$E179WVS[is.na(povo$E179WVS)] <- -55
table(povo$E179WVS)

povo$family <- factor(
  povo$E179WVS,
  levels = c(76007,76001,76009,76010,76029,76015,
             76023,76024,76025,76004,76002,
             76003,76012,76014),
  labels = c("Nacionalista/Democ.Cristão/Liberal",
             "Social Democrata","Comunista/Ecológico",
             "Comunista/Ecológico","Comunista/Ecológico",
             "Nacionalista/Democ.Cristão/Liberal",
             "Nacionalista/Democ.Cristão/Liberal",
             "Nacionalista/Democ.Cristão/Liberal",
             "Nacionalista/Democ.Cristão/Liberal",
             "Social Democrata",
             "Social Democrata",
             "Social Democrata",
             "Social Democrata",
             "Social Democrata"))
table(povo$family)
library(haven)
dffull <-read_sav("BASEDATOS_BRASIL_102.sav")
df <- remove_labels(dffull)
print(dffull$partido)
table(df$partido)
df$family <- factor(
  df$partido,
  levels = c(492,493,502,504,495,497,498,500,509,494,496,499,501,503),
  labels = c("Nacionalista/Democ.Cristão/Liberal",
             "Social Democrata",
             "Comunista/Ecológico","Comunista/Ecológico",
             "Nacionalista/Democ.Cristão/Liberal",
             "Nacionalista/Democ.Cristão/Liberal",
             "Nacionalista/Democ.Cristão/Liberal",
             "Nacionalista/Democ.Cristão/Liberal",
             "Nacionalista/Democ.Cristão/Liberal",
             "Social Democrata",
             "Social Democrata",
             "Social Democrata",
             "Social Democrata",
             "Social Democrata"))

table(df$family)




#
library(labelled)

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

df <- subset(df, select=c(VAL2,VAL6,proMercado,family))

df->dffull
complete5 <- function(...) {
  study <- dffull %>%
    select(...)
  
  return(study[complete.cases(study),])
}

df <- complete5(VAL2,VAL6,proMercado,family)#como nao tem NA no proMercado
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
table(df$family)
df <- subset(df, select=c(family,proMercado,fundamentalismo))
df$family <- as.factor(df$family)
df$fundamentalismo <- scales::rescale(df$fundamentalismo, to = c(0, 1))
summary(df)

#
povo <- subset(povo, select=c(F118,F120,F028,F121,proMercado,family))
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

povo <- complete52(F118,F120,F028,F121,proMercado,family)#como nao tem NA no proMercado
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
table(povo$family)
povo <- subset(povo, select=c(family,proMercado,fundamentalismo))
povo$family <- as.factor(povo$family)
povo$fundamentalismo <- scales::rescale(povo$fundamentalismo, to = c(0, 1))

#
#########
#descritivas

#povo
summary(povo)


#   
#elite
summary(df)


#comparador medias e medianas
#media
tapply(povo$proMercado, povo$family, mean)#povo
tapply(df$proMercado, df$family, mean)#elite


#media
tapply(povo$fundamentalismo, povo$family, mean)#povo
tapply(df$fundamentalismo, df$family, mean)#elite


#microgap



#
#macro gap proMercado
mean_povo_proMercado <- mean(povo$proMercado)
mean_elite_proMercado <- mean(df$proMercado)
macroGap_proMercado <- abs(mean_povo_proMercado-mean_elite_proMercado)
macroGap_proMercado#

# macro de cada family
familys <- unique(df$family)

macroGap_proMercado <- sapply(familys, function(family) {
  mean_povo_proMercado <- mean(povo$proMercado[povo$family == family])
  mean_elite_proMercado <- mean(df$proMercado[df$family == family])
  abs(mean_povo_proMercado - mean_elite_proMercado)
})

result <- data.frame(family = familys, macroGap_proMercado = macroGap_proMercado)


# Resultado
print(result)#aqui!

#microGap proMercado
povo$microGap_proMercado <- abs(povo$proMercado - mean_elite_proMercado)
mean(povo$microGap_proMercado)
# Calcular a média de povo$microGap_proMercado por povo$family
mean_microgap_by_family_proMercado <- tapply(povo$microGap_proMercado, povo$family, mean)

# Resultado
print(mean_microgap_by_family_proMercado)#aqui!



#macro gap fundamentalismo
mean_povo_fundamentalismo <- mean(povo$fundamentalismo)
mean_elite_fundamentalismo <- mean(df$fundamentalismo)
macroGap_fundamentalismo <- abs(mean_povo_fundamentalismo-mean_elite_fundamentalismo)
macroGap_fundamentalismo#

# macro de cada family
familys <- unique(df$family)

#
macroGap_fundamentalismo <- sapply(familys, function(family) {
  mean_povo_fundamentalismo <- mean(povo$fundamentalismo[povo$family == family])
  mean_elite_fundamentalismo <- mean(df$fundamentalismo[df$family == family])
  abs(mean_povo_fundamentalismo - mean_elite_fundamentalismo)
})

result <- data.frame(family = familys, macroGap_fundamentalismo = macroGap_fundamentalismo)

#resultado macro gap por family
print(result)#aqui!


#microGap fundamentalismo
povo$microGap_fundamentalismo <- abs(povo$fundamentalismo - mean_elite_fundamentalismo)
mean(povo$microGap_fundamentalismo)
# Calcular a média de povo$microGap_fundamentalismo por povo$family
mean_microgap_by_family_fundamentalismo <- tapply(povo$microGap_fundamentalismo, povo$family, mean)

# Resultado
print(mean_microgap_by_family_fundamentalismo)#aqui!

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

