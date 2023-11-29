#2017-2018
library(dplyr)
library(ggplot2)
library(gridExtra)
library(readr)
library(labelled)
rm(list=ls())

povo <- read_csv("df7.csv")
library(haven)
povo$E179WVS[is.na(povo$E179WVS)] <- -55
table(povo$E179WVS)# abaixo os com mais de 20 , falta ver se tem pelo menos 3 deputados
#partidos WVS
#76001	BRA:Brazilian Democratic Movement = + q 20
#76002	BRA:Workers´ Party = + q 20
#76003	BRA:Brazilian Social Democracy Party = + q 20
#76028	FORA = BRA:Sustainability Network = + q 20 - REDE - nao tem deputados suficient (só 2)
#76029	BRA:Socialism and Liberty Party = + q 20


load("BLS8_Data.RData")
df <- bls[bls$wave == 2017, ]
rm(bls)
table(df$wave)

table(df$party_survey)
#13	PT - 18 deputados ok 
#15	PMDB - 16 deputados ok
# 18	REDE - nao tem deputados suficient (só 2)
# 45	PSDB 22 deputados OK
# 50	PSOL # 4 deputados OK

#publico
povo$partido <- NA
povo$partido[povo$E179WVS == 76001] <- "PMDB"
povo$partido[povo$E179WVS == 76002] <- "PT"
povo$partido[povo$E179WVS == 76003] <- "PSDB"
povo$partido[povo$E179WVS == 76029] <- "PSOL"
table(povo$partido, useNA = "always")
#elite
df$partido <- NA
# Substituindo valores específicos de acordo com a tabela fornecida
df$partido[df$party_survey == 15] <- "PMDB"
df$partido[df$party_survey == 13] <- "PT"
df$partido[df$party_survey == 45] <- "PSDB"
df$partido[df$party_survey == 50] <- "PSOL"
table(df$partido, useNA="always")



#arrumar povo primeiro 
#econ
summary(povo$E036)#remover NAS depois
povo$E036[povo$E036<0]<-NA
-1*povo$E036 -> povo$proMercado
table(povo$proMercado, useNA = "always")
povo$proMercado <- scales::rescale(povo$proMercado, to = c(0, 1))
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

povo <- complete52(F118,F120,F028,F121,proMercado,partido)#como nao tem NA no proMercado
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
table(povo$partido)
povo <- subset(povo, select=c(partido,proMercado,fundamentalismo))
povo$partido <- as.factor(povo$partido)
povo$fundamentalismo <- scales::rescale(povo$fundamentalismo, to = c(0, 1))
summary(povo)

#
#
# agora elite
#economia
table(df$inicpriv)#+10 mais estatista
df$inicpriv[df$inicpriv == -999] <- NA
table(df$inicpriv, useNA = "always")#
-1*df$inicpriv -> df$proMercado
df$proMercado <- scales::rescale(df$proMercado, to = c(0, 1))
table(df$proMercado, useNA = "always")

#
#fundamentalismo
table(df$casament)#5 strongly agree - INVERTER!
df$casament[df$casament == -999] <- NA
table(df$aborto)#5 strongly agree
df$aborto[df$aborto == -999] <- NA
-1*df$casament -> df$casament_invertido
df$casament_invertido <- scales::rescale(df$casament_invertido, to = c(0, 1))


#

df <- subset(df, select=c(casament_invertido,aborto,proMercado,partido))

df->dffull
summary(dffull)

complete5 <- function(...) {
  study <- dffull %>%
    select(...)
  
  return(study[complete.cases(study),])
}

df <- complete5(casament_invertido,aborto,proMercado,partido)#como nao tem NA no proMercado
summary(df)

library(psych)
library(semTools)
library(lavaan)
scree(df[,1:3])
pcaelite <-psych::principal(df[,1:3],2)
pcaelite#PRA TESTAR

model <- '
  # Especificação do modelo fatorial
  LiberalFundamentalismo =~ aborto+casament_invertido
  F2 =~ proMercado
 
  # Especificação das variâncias dos erros
  casament_invertido ~~ casament_invertido
  aborto ~~ aborto
  proMercado ~~ proMercado


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
merge$ano <- "2017-2018"
write.csv(merge,
          "C:/Users/grego/OneDrive/Desktop/work/Congruencia so brasil/distri geral/distribuni/df_t3.csv")
