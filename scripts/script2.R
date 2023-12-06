
# Observações iniciais ####

# Esse script é destinado a replicar as figuras 4,5,6,7,8,9 e 10
# para facilitar é sugerido que coloque os bancos de dados em uma mesma pasta e crie um projeto para rodar tudo junto

# a função complete5 as vezes da erro tem q reiniciar o projeto, aí funciona

rm(list=ls())
library(dplyr)
library(ggplot2)
library(gridExtra)
library(readr)
library(labelled)
library(psych)
library(lavaan)
library(semTools)


#começamos mostrando como geramos micros e macrogaps por período

# 2005-2006 ####
povo <- read_csv("df5.csv")
library(haven)
dffull <-read_sav("BASEDATOS_BRASIL_55.sav")
df <- remove_labels(dffull)
df$partido -> df$partido_formatoriginal
#publico
povo$partido <- NA
povo$partido[povo$E179WVS == 76001] <- "PMDB"
povo$partido[povo$E179WVS == 76002] <- "PT"
povo$partido[povo$E179WVS == 76003] <- "PSDB"
povo$partido[povo$E179WVS == 76004] <- "PDT"
povo$partido[povo$E179WVS == 76006] <- "PFL"
povo$partido[povo$E179WVS == 76007] <- "PTB"
table(povo$partido, useNA = "always")
#elite
df$partido <- NA
# Substituindo valores específicos de acordo com a tabela fornecida
df$partido[df$partido_formatoriginal == 2] <- "PMDB"
df$partido[df$partido_formatoriginal == 1] <- "PT"
df$partido[df$partido_formatoriginal == 6] <- "PSDB"
df$partido[df$partido_formatoriginal == 10] <- "PDT"
df$partido[df$partido_formatoriginal == 3] <- "PFL"
df$partido[df$partido_formatoriginal == 5] <- "PTB"
table(df$partido, useNA="always")

table(povo$E036)
# Recodificação recodifiquei 1 e 2 como 1; 3 e 4 como 2; 5 e 6 como 3; 7 e 8 como 4; e 9 e 10 como 5 - para ficar igual a base elite
str(povo$E036)
povo$E036 <- cut(povo$E036, breaks = c(0, 2, 4, 6, 8, 10), labels = c(1, 2, 3, 4, 5), include.lowest = TRUE)
povo$E036 <- as.numeric(povo$E036)
table(povo$E036, useNA = "always")

-1*povo$E036 -> povo$proMercado#para ficar no mesmo sentido da outra base
print(dffull$p28)#nao tem missing
df$proMercado <- df$p28
df$proMercado <- scales::rescale(df$proMercado, to = c(0, 1))
povo$proMercado <- scales::rescale(povo$proMercado, to = c(0, 1))
table(df$proMercado, useNA = "always")
table(povo$proMercado, useNA = "always")
#fundamentalismo
print(dffull$p60b)#10 máxima religiosidade-- posicao fundamentalista
print(dffull$p64)#10 posição liberal - SEM NA (divórcio) # 
print(dffull$p65)#10 contra aborto - posicao fundamentalista


hist(df$p65)

povo$F1201 <- -1*povo$F120
hist(povo$F1201)
df$p65[df$p65==99]<-NA
df$p64[df$p64== 99]<-NA
df$p64 <- -1*df$p64#inverter
df$p60b#NA já cadastrados
summary(df$p64)#poucos NA
summary(df$p65)#poucos NA
summary(df$p60b)#Muitos NAS nao usar 


df <- subset(df, select=c(p60b,p64,p65,proMercado,partido))#remover NAS
summary(df)
df->dffull
complete5 <- function(...) {
  study <- dffull %>%
    select(...)
  
  return(study[complete.cases(study),])
}

df <- complete5(p60b,p64,p65,proMercado,partido)#como nao tem NA no proMercado
summary(df)
scree(df[,1:4])
pcaelite <-psych::principal(df[,1:4],2)
pcaelite#PRA TESTAR

model <- '
  # Especificação do modelo fatorial
  LiberalFundamentalismo =~ p60b + p64 + p65
 
  # Especificação das variâncias dos erros
  p60b ~~ p60b
  p64 ~~ p64
  p65 ~~ p65


'
# Ajuste do modelo de CFA
fit <- cfa(model, data = df)

summary(fit)
semTools::fitmeasures(fit, c("tli", "cfi", "rmsea", "srmr"))#
scores <- lavPredict(fit)
scores[,1] -> df$fundamentalismo
#

rm(model,scores,fit)
table(df$partido)
df <- subset(df, select=c(partido,proMercado,fundamentalismo))
df$partido <- as.factor(df$partido)
df$fundamentalismo <- scales::rescale(df$fundamentalismo, to = c(0, 1))
#

povo <- subset(povo, select=c(F118,F120,F028,F121,proMercado,partido))
summary(povo)
table(povo$F118)
povo$F118[povo$F118<0]<-NA
povo$F120[povo$F120<0]<-NA
povo$F121[povo$F121<0]<-NA
povo$F028[povo$F028<0]<-NA




#print(df7$F118)#liberal (valores positivos)CasGay
#print(df7$F120)#liberal (valores positivos)Aborto
#print(df7$F028)#valores positivos pouca religiosidade
#print(df7$F121)#valores positivos liberal (divorce)
#todos os valores positivos são liberal, basta inverter no score
#inverter no score(lembrete 1)


povofull <- povo


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

#só aqui, verificação de se a função sapply deu certo
print(result)#3    PSDB         0.01006494
psdb_df <- subset(df, partido == "PSDB") #fiz um manual para testar se minha função sapply deu certo
psdb_povo <- subset(povo, partido == "PSDB")
mean_psdb_povo_proMercado <- mean(psdb_povo$proMercado)
mean_elite_psdb_proMercado <- mean(psdb_df$proMercado)
macroGap_psdb_proMercado <- abs(mean_psdb_povo_proMercado-mean_elite_psdb_proMercado)
macroGap_psdb_proMercado# [1] 0.01006494 ;  bateu? Si, usar a função nas próximas situações



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
#


summary(povo)
povo$microGap_fundamentalismo
povo_fundamentalismo <- subset(povo, select=c(partido,microGap_fundamentalismo))
povo_fundamentalismo$Dimensão <- "Liberal / Fundamentalismo adaptado"
povo_fundamentalismo$Período <- "2005-2006"
povo_fundamentalismo$microGap_fundamentalismo -> povo_fundamentalismo$microGap
povo_fundamentalismo <- subset(povo_fundamentalismo, select=c(partido, microGap, Dimensão,Período))

summary(povo_fundamentalismo)
write.csv(povo_fundamentalismo,
          "fund05_06.csv")
povo_merca<- subset(povo, select=c(partido,microGap_proMercado))
povo_merca$Dimensão <- "Pró Estado V.S Pró Mercado"
povo_merca$Período <- "2005-2006"
povo_merca$microGap_proMercado -> povo_merca$microGap
povo_merca <- subset(povo_merca, select=c(partido, microGap, Dimensão,Período))

write.csv(povo_merca,
          "mer05_06.csv")

#
# Realizar o teste t para a variável "fundamentalismo"
t_test_fundamentalismo <- t.test(df$fundamentalismo, povo$fundamentalismo)

# Realizar o teste t para a variável "proMercado"
t_test_proMercado <- t.test(df$proMercado, povo$proMercado)

# Exibir os resultados
print(t_test_fundamentalismo)
print(t_test_proMercado)
t_test_proMercado -> t_test_proMercado_t1
t_test_fundamentalismo -> t_test_fundamentalismo_t1
minha_lista <- list(t_test_proMercado_t1 = t_test_proMercado_t1,
                    t_test_fundamentalismo_t1 = t_test_fundamentalismo_t1)

#

povo$base <- "Público"
df$base <- "Elite"
merge <- full_join(povo,df)
merge$ano <- "2005-2006"
write.csv(merge,
          "df_t1.csv")




# 2014 -2014 ####
rm(list = setdiff(ls(), "minha_lista"))
povo <- read_csv("df6.csv")
dffull <-read_sav("BASEDATOS_BRASIL_102.sav")
df <- remove_labels(dffull)
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

povo$E036 <- cut(povo$E036, breaks = c(0, 2, 4, 6, 8, 10), labels = c(1, 2, 3, 4, 5), include.lowest = TRUE)
povo$E036 <- as.numeric(povo$E036)
table(povo$E036, useNA = "always")
summary(povo$E036)
-1*povo$E036 -> povo$proMercado
print(dffull$EM1)#10 mercado
table(df$EM1, useNA = "always")# Um na arrumar depois
df$proMercado <- df$EM1
df$proMercado <- scales::rescale(df$proMercado, to = c(0, 1))
povo$proMercado <- scales::rescale(povo$proMercado, to = c(0, 1))
#fundamentalismo, as que tem
print(dffull$VAL2)#cas gay
table(df$VAL2)#10 é favor - inverter
print(dffull$VAL6)#aborto
table(df$VAL6)# 10 é favor - inverter

#inverter no scores

df <- subset(df, select=c(VAL2,VAL6,proMercado,partido))

df->dffull
complete5 <- function(...) {
  study <- dffull %>%
    select(...)
  
  return(study[complete.cases(study),])
}

df <- complete5(VAL2,VAL6,proMercado,partido)#como nao tem NA no proMercado
summary(df)
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
table(df$partido)
df <- subset(df, select=c(partido,proMercado,fundamentalismo))
df$partido <- as.factor(df$partido)
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


df$base <- "Elite" 
povo$base <- "Povo"



povo$microGap_fundamentalismo -> povo$microGap
povo_fundamentalismo <- subset(povo, select=c(partido,microGap))
povo_fundamentalismo$Dimensão <- "Liberal / Fundamentalismo adaptado"
povo_fundamentalismo$Período <- "2014-2014"
summary(povo_fundamentalismo)
write.csv(povo_fundamentalismo,
          "fund14_14.csv")
povo_merca<- subset(povo, select=c(partido,microGap_proMercado))
povo_merca$Dimensão <- "Pró Estado V.S Pró Mercado"
povo_merca$Período <- "2014-2014"
povo_merca$microGap_proMercado -> povo_merca$microGap
povo_merca <- subset(povo_merca, select=c(partido, microGap, Dimensão,Período))

write.csv(povo_merca,
          "mer14_14.csv")

#
# Realizar o teste t para a variável "fundamentalismo"
t_test_fundamentalismo <- t.test(df$fundamentalismo, povo$fundamentalismo)

# Realizar o teste t para a variável "proMercado"
t_test_proMercado <- t.test(df$proMercado, povo$proMercado)

# Exibir os resultados
print(t_test_fundamentalismo)
print(t_test_proMercado)
t_test_proMercado -> t_test_proMercado_t2
t_test_fundamentalismo -> t_test_fundamentalismo_t2
minha_lista <- c(minha_lista,
                 list(t_test_proMercado_t2=t_test_proMercado_t2,
                      t_test_fundamentalismo_t2=t_test_fundamentalismo_t2))



#

povo$base <- "Público"
df$base <- "Elite"
merge <- full_join(povo,df)
merge$ano <- "2014-2014"
write.csv(merge,
          "df_t2.csv")

# 2017-2018 ####

rm(list = setdiff(ls(), "minha_lista"))
povo <- read_csv("df7.csv")
load("BLS8_Data.RData")
df <- bls[bls$wave == 2017, ]
rm(bls)
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
#print(df7$F118)#liberal (valores positivos)CasGay
#print(df7$F120)#liberal (valores positivos)Aborto
#print(df7$F028)#valores positivos pouca religiosidade
#print(df7$F121)#valores positivos liberal (divorce)
#todos os valores positivos são liberal, basta inverter no score
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

summary(povo)
povo$microGap_fundamentalismo -> povo$microGap
povo_fundamentalismo <- subset(povo, select=c(partido,microGap))
povo_fundamentalismo$Dimensão <- "Liberal / Fundamentalismo adaptado"
povo_fundamentalismo$Período <- "2017-2018"
summary(povo_fundamentalismo)
write.csv(povo_fundamentalismo,
          "fund17_18.csv")
povo_merca<- subset(povo, select=c(partido,microGap_proMercado))
povo_merca$Dimensão <- "Pró Estado V.S Pró Mercado"
povo_merca$Período <- "2017-2018"
povo_merca$microGap_proMercado -> povo_merca$microGap
povo_merca <- subset(povo_merca, select=c(partido, microGap, Dimensão,Período))

write.csv(povo_merca,
          "mer17_18.csv")

#
# Realizar o teste t para a variável "fundamentalismo"
t_test_fundamentalismo <- t.test(df$fundamentalismo, povo$fundamentalismo)

# Realizar o teste t para a variável "proMercado"
t_test_proMercado <- t.test(df$proMercado, povo$proMercado)

# Exibir os resultados
print(t_test_fundamentalismo)
print(t_test_proMercado)
t_test_proMercado -> t_test_proMercado_t3
t_test_fundamentalismo -> t_test_fundamentalismo_t3
minha_lista <- c(minha_lista,
                 list(t_test_proMercado_t3=t_test_proMercado_t3,
                      t_test_fundamentalismo_t3=t_test_fundamentalismo_t3))

#

povo$base <- "Público"
df$base <- "Elite"
merge <- full_join(povo,df)
merge$ano <- "2017-2018"
write.csv(merge,
          "df_t3.csv")

# Out 2018 (party elected) - 2019 ####
rm(list = setdiff(ls(), "minha_lista"))
povofull <- read_sav("Banco de Dados A Cara da Democracia 2019.sav")
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
pcaelite #pcapovo em 2021_BLS_2019_ADC

#   
#elite
summary(df)
#summary(povo) em 2021_BLS_2019_ADC

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
summary(povo)
povo$microGap_fundamentalismo -> povo$microGap
povo_fundamentalismo <- subset(povo, select=c(partido,microGap))
povo_fundamentalismo$Dimensão <- "Liberal / Fundamentalismo adaptado"
povo_fundamentalismo$Período <- "Out 2018-2019"
summary(povo_fundamentalismo)
write.csv(povo_fundamentalismo,
          "fundelected18_19.csv")

#merelected18_19.csv
povo_merca<- subset(povo, select=c(partido,microGap_proMercado))
povo_merca$Dimensão <- "Pró Estado V.S Pró Mercado"
povo_merca$Período <- "Out 2018-2019"
povo_merca$microGap_proMercado -> povo_merca$microGap
povo_merca <- subset(povo_merca, select=c(partido, microGap, Dimensão,Período))
summary(povo_merca)
write.csv(povo_merca,
          "merelected18_19.csv")


#
#
# Realizar o teste t para a variável "fundamentalismo"
t_test_fundamentalismo <- t.test(df$fundamentalismo, povo$fundamentalismo)

# Realizar o teste t para a variável "proMercado"
t_test_proMercado <- t.test(df$proMercado, povo$proMercado)

# Exibir os resultados
print(t_test_fundamentalismo)
print(t_test_proMercado)

t_test_proMercado -> t_test_proMercado_t4
t_test_fundamentalismo -> t_test_fundamentalismo_t4
minha_lista <- c(minha_lista,
                 list(t_test_proMercado_t4=t_test_proMercado_t4,
                      t_test_fundamentalismo_t4=t_test_fundamentalismo_t4))
#

povo$base <- "Público"
df$base <- "Elite"
merge <- full_join(povo,df)
merge$ano <- "Out 2018-2019"
write.csv(merge,
          "df_t4.csv")

# 2021- 2019 #######
rm(list = setdiff(ls(), "minha_lista"))
povofull <- read_sav("Banco de Dados A Cara da Democracia 2019.sav")
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


povo$microGap_fundamentalismo -> povo$microGap
povo_fundamentalismo <- subset(povo, select=c(partido,microGap))
povo_fundamentalismo$Dimensão <- "Liberal / Fundamentalismo adaptado"
povo_fundamentalismo$Período <- "2021-2019"
summary(povo_fundamentalismo)
write.csv(povo_fundamentalismo,
          "fund21_19.csv")

#merelected18_19.csv
povo_merca<- subset(povo, select=c(partido,microGap_proMercado))
povo_merca$Dimensão <- "Pró Estado V.S Pró Mercado"
povo_merca$Período <- "2021-2019"
povo_merca$microGap_proMercado -> povo_merca$microGap
povo_merca <- subset(povo_merca, select=c(partido, microGap, Dimensão,Período))
summary(povo_merca)
write.csv(povo_merca,
          "mer21_19.csv")
#

#
# Realizar o teste t para a variável "fundamentalismo"
t_test_fundamentalismo <- t.test(df$fundamentalismo, povo$fundamentalismo)

# Realizar o teste t para a variável "proMercado"
t_test_proMercado <- t.test(df$proMercado, povo$proMercado)

# Exibir os resultados
print(t_test_fundamentalismo)
print(t_test_proMercado)
t_test_proMercado -> t_test_proMercado_t5
t_test_fundamentalismo -> t_test_fundamentalismo_t5
minha_lista <- c(minha_lista,
                 list(t_test_proMercado_t5=t_test_proMercado_t5,
                      t_test_fundamentalismo_t5=t_test_fundamentalismo_t5))

#

povo$base <- "Público"
df$base <- "Elite"
merge <- full_join(povo,df)
merge$ano <- "2021-2019"
write.csv(merge,
          "df_t5.csv")


# Figura 4 e 5 ####
rm(list = setdiff(ls(), "minha_lista"))
library(see)
library(readxl)
#criado com base nos macrogaps obtidos nas etapas anteriores, cadastro manual
# Criando o dataframe manualmente
df <- data.frame(
  var1 = c(
    "PT", "PMDB", "PFL", "PSDB", "PTB", "PDT",
    "PT", "PMDB", "PSDB", "PSB", "PDT",
    "PMDB", "PSDB", "PSOL", "PT",
    "MDB", "PSL", "PT",
    "MDB", "PSL", "PT",
    "PT", "PMDB", "PFL", "PSDB", "PTB", "PDT",
    "PT", "PMDB", "PSDB", "PSB", "PDT",
    "PMDB", "PSDB", "PSOL", "PT",
    "MDB", "PSL", "PT",
    "MDB", "PSL", "PT"
  ),
  var2 = c(
    "2005/2006", "2005/2006", "2005/2006", "2005/2006", "2005/2006", "2005/2006",
    "2014/2014", "2014/2014", "2014/2014", "2014/2014", "2014/2014",
    "2017/2018", "2017/2018", "2017/2018", "2017/2018",
    "Out 2018/2019", "Out 2018/2019", "Out 2018/2019",
    "2021/2019", "2021/2019", "2021/2019",
    "2005/2006", "2005/2006", "2005/2006", "2005/2006", "2005/2006", "2005/2006",
    "2014/2014", "2014/2014", "2014/2014", "2014/2014", "2014/2014",
    "2017/2018", "2017/2018", "2017/2018", "2017/2018",
    "Out 2018/2019", "Out 2018/2019", "Out 2018/2019",
    "2021/2019", "2021/2019", "2021/2019"
  ),
  var3 = c(
    0.136, 0.223, 0.132, 0.006, 0.335, 0.243,
    0.279, 0.118, 0.078, 0.154, 0.07,
    0.18, 0.228, 0.568, 0.014,
    0.273, 0.362, 0.073,
    0.328, 0.384, 0.07,
    0.269, 0.243, 0.262, 0.297, 0.066, 0.406,
    0.388, 0.071, 0.075, 0.1, 0.25,
    0.184, 0.102, 0.449, 0.422,
    0.486, 0.062, 0.524,
    0.506, 0.127, 0.544
  ),
  var4 = c(
    "Pró Estado / Pró Mercado (solo)", "Pró Estado / Pró Mercado (solo)",
    "Pró Estado / Pró Mercado (solo)", "Pró Estado / Pró Mercado (solo)",
    "Pró Estado / Pró Mercado (solo)", "Pró Estado / Pró Mercado (solo)",
    "Pró Estado / Pró Mercado (solo)", "Pró Estado / Pró Mercado (solo)",
    "Pró Estado / Pró Mercado (solo)", "Pró Estado / Pró Mercado (solo)",
    "Pró Estado / Pró Mercado (solo)", "Pró Estado / Pró Mercado (solo)",
    "Pró Estado / Pró Mercado (solo)", "Pró Estado / Pró Mercado (solo)",
    "Pró Estado / Pró Mercado (solo)", "Pró Estado / Pró Mercado (solo)",
    "Pró Estado / Pró Mercado (solo)", "Pró Estado / Pró Mercado (solo)",
    "Pró Estado / Pró Mercado (solo)", "Pró Estado / Pró Mercado (solo)",
    "Pró Estado / Pró Mercado (solo)", "Liberal / Fundamentalismo",
    "Liberal / Fundamentalismo", "Liberal / Fundamentalismo",
    "Liberal / Fundamentalismo", "Liberal / Fundamentalismo",
    "Liberal / Fundamentalismo", "Liberal / Fundamentalismo",
    "Liberal / Fundamentalismo", "Liberal / Fundamentalismo",
    "Liberal / Fundamentalismo", "Liberal / Fundamentalismo",
    "Liberal / Fundamentalismo", "Liberal / Fundamentalismo",
    "Liberal / Fundamentalismo", "Liberal / Fundamentalismo",
    "Liberal / Fundamentalismo", "Liberal / Fundamentalismo",
    "Liberal / Fundamentalismo", "Liberal / Fundamentalismo",
    "Liberal / Fundamentalismo", "Liberal / Fundamentalismo"
  )
)

# Exibindo o dataframe
print(df)



df$var4 -> df$Dimensão


df$var1 <- memisc::recode(as.factor(df$var1),
                          "MDB/PMDB" <- c("MDB","PMDB"),
                          "PT" <- c("PT"),
                          "PSDB" <- c("PSDB"),
                          "PDT" <- c("PDT"),
                          "PSL" <- c("PSL"),
                          "PSB" <- c("PSB"),
                          "PFL" <- c("PFL"),
                          "PSOL" <- c("PSOL"),
                          "PTB" <- c("PTB"))
table(df$var1)

ordem_desejada <- c("MDB/PMDB","PT",
                    "PSDB", "PSL", "PDT","PSB","PFL","PSOL", "PTB")
df$var1 <- factor(df$var1, levels = ordem_desejada)

table(df$var1)

table(df$var2)
ordem_desejada <- c("2005/2006",
                    "2014/2014","2017/2018",
                    "Out 2018/2019", "2021/2019")
df$var2 <- factor(df$var2, levels = ordem_desejada)
table(df$var2)

df_filtrado2 <- subset(df, !(var1 %in% c("PSB", "PFL", "PSOL", "PTB")))
table(df_filtrado2$var1)
df_filtrado2 <- droplevels(df_filtrado2)

cores_manual <- c("#333333", "#CCCCCC")
# Criar o gráfico com os dados filtrados
graf<-   ggplot(df_filtrado2,aes(var2, var3, group=Dimensão)) +
  geom_line(aes(colour = Dimensão),size=2) +
  geom_point(aes(colour = Dimensão),size=3) +
  facet_wrap(~ var1) +
  labs(y = "MacroGap", x = "Período") +
  theme_bw() +
  theme(legend.position = "bottom")+
  scale_color_manual(values=cores_manual)


graf# Figura 5

macrao <- read_excel("macrao.xlsx"
                    )

# macrao tem o macro gap geral (sem levar em consideração partidos 
# e foi construído da mesma forma)

head(macrao, 10)
# Criar dataframe manualmente
macrao <- data.frame(
  var32 = c("2005/2006", "2014/2014", "2017/2018", "Out 2018/2019", "2021/2019", "2005/2006", "2014/2014", "2017/2018", "Out 2018/2019", "2021/2019"),
  var33 = c("Liberal/Fundamentalismo", "Liberal/Fundamentalismo", "Liberal/Fundamentalismo", "Liberal/Fundamentalismo", "Liberal/Fundamentalismo", "Pró Estado/Pró Mercado", "Pró Estado/Pró Mercado", "Pró Estado/Pró Mercado", "Pró Estado/Pró Mercado", "Pró Estado/Pró Mercado"),
  var34 = c(0.247, 0.193, 0.234, 0.355, 0.377, 0.097, 0.043, 0.145, 0.134, 0.156)
)

# Visualizar o dataframe
head(macrao,10)


macrao$var33 -> macrao$Dimensão
ordem_desejada <- c("2005/2006",
                    "2014/2014","2017/2018",
                    "Out 2018/2019", "2021/2019")

macrao$var32 <- factor(macrao$var32, levels = ordem_desejada)

# Criar o gráfico com os dados filtrados
graf <- ggplot(macrao, aes(var32, var34, group=Dimensão)) +
  geom_line(aes(colour = Dimensão),size=2) +
  geom_point(aes(colour = Dimensão),size=4) +
  #facet_wrap(~ var33) +
  labs(y = "MacroGap", x = "Período") +
  theme(legend.position = "bottom")
cores_manual <- c("#333333", "#CCCCCC")
# Exibir o gráfico
graf + theme_bw()+theme(legend.position = "bottom")+scale_color_manual(values=cores_manual)

# Figura 7 e 8 #########

rm(list = setdiff(ls(), "minha_lista"))
mer1 <- read.csv("mer05_06.csv")
table(mer1$Dimensão)#pra testar
table(mer1$Período)#pra testar
table(mer1$partido)#pra testar, ok tem dobrado por ter unificado as bases
mer2 <- read.csv("mer14_14.csv")
mer3 <- read.csv("mer17_18.csv")
mer4 <- read.csv("merelected18_19.csv")
mer5 <- read.csv("mer21_19.csv")
fund1 <- read.csv("fund05_06.csv")
fund2 <- read.csv("fund14_14.csv")
fund3 <- read.csv("fund17_18.csv")
fund4 <- read.csv("fundelected18_19.csv")
fund5 <- read.csv("fund21_19.csv")
merge <- full_join(mer1,fund1)
table(merge$partido)
table(merge$Dimensão)
merge <- full_join(merge,mer2)
merge <- full_join(merge,fund2)
merge <- full_join(merge,mer3)
merge <- full_join(merge,fund3)
merge <- full_join(merge,mer4)
merge <- full_join(merge,fund4)
merge <- full_join(merge,mer5)
merge <- full_join(merge,fund5)


summary(merge)
table(merge$Período)
ordem_desejada <- c("2005-2006",
                    "2014-2014","2017-2018",
                    "Out 2018-2019", "2021-2019")
merge$Período <- factor(merge$Período, levels = ordem_desejada)
table(merge$Período)
table(merge$Dimensão)
table(merge$partido, merge$Período)
table(merge$Dimensão, merge$Período)

library(memisc)

merge$partido <- memisc::recode(as.factor(merge$partido),
                                "MDB/PMDB" <- c("MDB","PMDB"),
                                "PT" <- c("PT"),
                                "PSDB" <- c("PSDB"),
                                "PDT" <- c("PDT"),
                                "PSL" <- c("PSL"),
                                "PSB" <- c("PSB"),
                                "PFL" <- c("PFL"),
                                "PSOL" <- c("PSOL"),
                                "PTB" <- c("PTB"))
table(merge$partido)

ordem_desejada <- c("MDB/PMDB","PT",
                    "PSDB", "PSL", "PDT","PSB","PFL","PSOL", "PTB")
merge$partido <- factor(merge$partido, levels = ordem_desejada)
table(merge$partido)
library(ggdist)
library(tidyquant)
defato1 <- merge %>%
  dplyr::filter(Dimensão == "Liberal / Fundamentalismo adaptado") %>%
  ggplot(aes(x=microGap, y=Período, fill=Período))+
  #ggdist::stat_halfeye(adjust=0.5, justification = -.2, .width=0, point_colour=NA) +
  geom_boxplot(width=-.4,outlier.colour = NA,alpha=0.5)+
  #ggdist::stat_dots(side="left",justification=1.1, binwidth=.25) +
  scale_fill_tq()+ #facet_grid(.~ Período)+
  theme_tq() +
  labs(title= "",
       subtitle = "",
       x = "Micro Gap Liberal/Fundamentalismo",
       y= "Período",
       fill= "Período",
  ) + theme(legend.position = "none")

defato1 <- defato1 + stat_summary(fun = "mean", geom = "point", size = 3, shape = 21, fill = "white", colour = "black") 

defato1
defato2 <- merge %>%
  dplyr::filter(Dimensão == "Pró Estado V.S Pró Mercado") %>%
  ggplot(aes(x=microGap, y=Período, fill=Período))+
  #ggdist::stat_halfeye(adjust=0.5, justification = -.2, .width=0, point_colour=NA) +
  geom_boxplot(width=-.4,outlier.colour = NA,alpha=0.5)+
  #ggdist::stat_dots(side="left",justification=1.1, binwidth=.25) +
  scale_fill_tq()+ #facet_grid(.~ Período)+
  theme_tq() +
  labs(title= "",
       subtitle = "",
       x = "Micro Gap Pró Estado / Pró Mercado",
       y= "Período",
       fill= "Período",
  ) + theme(legend.position = "none")
defato2 <- defato2 + stat_summary(fun = "mean", geom = "point", size = 3, shape = 21, fill = "white", colour = "black") 


#
cores_manual <- c("#000000", "#333333", "#666666", "#999999", "#CCCCCC")

defato1+scale_fill_manual(values = cores_manual)
defato2+scale_fill_manual(values = cores_manual)


# # Figura 6 e 9 #####

rm(list = setdiff(ls(), "minha_lista"))
#recod
library(readr)
df_t1 <- read_csv("df_t1.csv")
df_t2 <- read_csv("df_t2.csv")
df_t3 <- read_csv("df_t3.csv")
df_t5 <- read_csv("df_t5.csv")
df_t4 <- read_csv("df_t4.csv")
library(tidyverse)
df <- full_join(df_t1,df_t2)
df <- full_join(df,df_t3)
df <- full_join(df,df_t4)
df <- full_join(df,df_t5)
summary(df)
table(df$base)
table(df$ano)
table(df$partido)
table(df$ano,df$partido)
df$ano <- as.factor(df$ano)
levels(df$ano)
# Defina a ordem desejada
ordem_desejada <- c("2005-2006", "2014-2014", "2017-2018", "Out 2018-2019", "2021-2019")

# Reordene a variável df$ano de acordo com a ordem desejada
df$ano <- factor(df$ano, levels = ordem_desejada)
levels(df$ano)


table(df$partido)
# Substitua "MDB" e "PMDB" por "MDB/PMDB"
df$partido[df$partido %in% c("MDB", "PMDB")] <- "MDB/PMDB"

# Defina a ordem desejada dos níveis
ordem_desejada_partido <- c("MDB/PMDB", "PT", "PSDB", "PSL", "PDT", "PSB", "PFL", "PSOL", "PTB")

# Reordene a variável "partido" de acordo com a ordem desejada
df$partido <- factor(df$partido, levels = ordem_desejada_partido)

# Verifique novamente a tabela
table(df$partido)


table(df$partido, df$ano)

# Crie o histograma com densidade ou percentagem para a dimensão cultural
df_fundamentalismo <- df %>% 
  dplyr::filter(ano != "Out 2018-2019") %>%
  ggplot(aes(x = fundamentalismo, y = ..density..)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(
    subtitle = "Dimensão cultural (Liberal-Fundamentalismo)",
    y = "",x="<<<Liberal / Fundamentalismo>>>              <<<Liberal / Fundamentalismo>>>",
    fill = "Ano"
  ) +
  facet_grid(ano ~ base) +
  theme_bw()

# Imprima o histograma da figura 6
df_fundamentalismo+theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
df_filtrado <- df[df$ano != "Out 2018-2019", ]#remover para ficar mais bunito
# Reordenar os níveis da variável partido da esquerda para a direita
df_filtrado$partido <- factor(df_filtrado$partido, levels = c("PSOL", "PT", "PDT", "PSB",
                                                              "MDB/PMDB", "PSDB", "PFL",
                                                              "PSL", "PTB"))
# Renomeie as colunas
df_filtrado$`microgap Pró Estado/Pró Mercado` <- df_filtrado$microGap_proMercado
df_filtrado$`microgap Liberal/Fundamentalismo` <- df_filtrado$microGap_fundamentalismo

# Reorganize os dados em formato longo
df_long <- df_filtrado %>%
  pivot_longer(cols = c(`microgap Liberal/Fundamentalismo`, `microgap Pró Estado/Pró Mercado`),
               names_to = "Medida",
               values_to = "Valor")
# Cores para fill
colors <- c('gray33', 'gray56', 'gray78', 'gray33', 'gray56', 'gray78', 'gray33', 'gray56', 'gray78')

combined_plot <- ggplot(df_long, aes(x = partido, y = Valor, fill = partido)) +
  geom_boxplot() +
  facet_grid(Medida ~ ano, scales = "free_y") +
  labs(
    subtitle = "Micro Gap Liberal / Fundamentalismo e Micro Gap Pró Mercado / Pró Estado",
    x = "",
    y = "",
    color = "Partido"
  ) +
  scale_fill_manual(values = colors, guide = "none") +  # Remover a legenda
  theme_bw() +
  coord_flip() +
  stat_summary(fun = "mean", geom = "point", size = 3, shape = 21, fill = "white", colour = "black")

print(combined_plot)


#Figura 10 ##########
#os dados foram obtidos dos t_test gerados 
#em cada ano e cadastrados manualmente

#a lista minha_lista serve para isso
### Figura 10 - parte A #####
minha_lista#conferir


options(scipen = 999)
# Dados para Liberal Fundamentalismo adaptado
periodos_liberal <- c("2005-2006", "2014-2014", "2018-2018", "2018 out-2019", "2021-2019")
diferencas_medias_liberal <- c(-0.2487305, -0.1937144, -0.2381865, -0.3555346, -0.3777312)
intervalos_confianca_inf_liberal <- c(-0.3004076, -0.2595775, -0.3126582, -0.4653881, -0.4810322)
intervalos_confianca_sup_liberal <- c(-0.1950533, -0.1278513, -0.1569708, -0.2462930, -0.2744302)
p_values_liberal <- c(0.00000000000001082, 0.00000008485, 0.00000007651, 0.00000005782, 0.00000000318)
significancia_liberal <- ifelse(p_values_liberal < 0.05, "Significativo", "Não Significativo")

# Criar um data frame para Liberal Fundamentalismo adaptado
dados_liberal <- data.frame(
  Periodo = factor(periodos_liberal, levels = periodos_liberal),
  Diferenca_Medias = diferencas_medias_liberal,
  Intervalo_Confianca_Inferior = intervalos_confianca_inf_liberal,
  Intervalo_Confianca_Superior = intervalos_confianca_sup_liberal,
  P_Value = p_values_liberal,
  Significância = significancia_liberal,
  Dimensao = "Fundamentalismo"
)

# Dados para Pró Estado / Pró Mercado
periodos_pro_estado <- c("2005-2006", "2014-2014", "2018-2018", "2018 out-2019", "2021-2019")
diferencas_medias_pro_estado <- c(0.0757808, -0.0472193, 0.1612923, 0.1374638, 0.1588924)
p_values_pro_estado <- c(0.00385, 0.2179, 0.001673, 0.04342, 0.02091)
significancia_pro_estado <- ifelse(p_values_pro_estado < 0.05, "Significativo", "Não Significativo")

intervalos_confianca_inf_pro_estado <- c(0.03192487, -0.1227022, 0.06269273, 0.004251945, 0.02526291)
intervalos_confianca_sup_pro_estado <- c(0.16163668, 0.0282634, 0.25967569, 0.270675642, 0.29252182)

# Criar um data frame para Pró Estado / Pró Mercado
dados_pro_estado <- data.frame(
  Periodo = factor(periodos_pro_estado, levels = periodos_pro_estado),
  Diferenca_Medias = diferencas_medias_pro_estado,
  P_Value = p_values_pro_estado,
  Significância = significancia_pro_estado,
  Intervalo_Confianca_Inferior = intervalos_confianca_inf_pro_estado,
  Intervalo_Confianca_Superior = intervalos_confianca_sup_pro_estado,
  Dimensao = "Pró Mercado"
)

# Combinar os dois data frames
dados_combinados <- rbind(dados_liberal, dados_pro_estado)

# Criar o gráfico com facet_wrap
grafico_combinado <- ggplot(dados_combinados, aes(x = Periodo, y = Diferenca_Medias, fill = Significância)) +
  geom_errorbar(aes(ymin = Intervalo_Confianca_Inferior, ymax = Intervalo_Confianca_Superior),
                width = 0.4, position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange",
               position = position_dodge(width = 0.9), color = "black", fill = "white") +
  labs(y = "Diferença de Médias (Elite - Povo)", x = "Período",
       subtitle = "") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Adiciona a linha vermelha
  facet_wrap(~ Dimensao, scales = "free_y") +
  scale_fill_manual(values = c("gray" = "gray", "Não Significativo" = "gray",
                               "Significativo" = "blue")) +
  theme_minimal()

grafico_combinado
library(ggdist)
library(tidyquant)
# tentando ver como fica virado
grafico_combinado + coord_flip()
grafico_combinado + coord_flip()+ theme_tq()
grafico_salvar <- grafico_combinado + coord_flip()+ theme_bw()
# acima temos com a nossa base dos minimos 20 partidarios e 3 deputados

# agora com a base completa
library(ggplot2)
options(scipen = 999)

### Figura 10 - parte B #####
#Fiz o mesmo procedimento mas para base toda (B)

# Dados para Liberal Fundamentalismo adaptado
periodos_liberal <- c("2005-2006", "2014-2014", "2017-2018", "Out 2018-2019", "2021-2019")
diferencas_medias_liberal <- c(
  0.4828713 - 0.6545586,
  0.4736155 - 0.6026740,
  0.4051059 - 0.5822967,
  0.3254265 - 0.5919041,
  0.3254265 - 0.5919041
)
intervalos_confianca_inf_liberal <- c(-0.2155155, -0.18212540, -0.2327726, -0.3235571, -0.3235571)
intervalos_confianca_sup_liberal <- c(-0.1278591, -0.07599148, -0.1216091, -0.2093980, -0.2093980)
p_values_liberal <- c(0.000000000002346, 0.000004099, 0.000000003145, 0.0000000000000005925, 0.0000000000000005925)
significancia_liberal <- ifelse(p_values_liberal < 0.05, "Significativo", "Não Significativo")

# Criar um data frame para Liberal Fundamentalismo adaptado
dados_liberal <- data.frame(
  Periodo = factor(periodos_liberal, levels = periodos_liberal),
  Diferenca_Medias = diferencas_medias_liberal,
  Intervalo_Confianca_Inferior = intervalos_confianca_inf_liberal,
  Intervalo_Confianca_Superior = intervalos_confianca_sup_liberal,
  P_Value = p_values_liberal,
  Significância = significancia_liberal,
  Dimensao = "Fundamentalismo"
)

# Dados para Pró Estado / Pró Mercado
periodos_pro_estado <- c("2005-2006", "2014-2014", "2017-2018", "Out 2018-2019", "2021-2019")
diferencas_medias_pro_estado <- c(
  0.6030702 - 0.5042827,
  0.5362319 - 0.5204765,
  0.7313433 - 0.5614836,
  0.6607143 - 0.4459855,
  0.6607143 - 0.4459855
)
p_values_pro_estado <- c(0.00385, 0.5865, 0.00000002274, 0.000000001759, 0.000000001759)
significancia_pro_estado <- ifelse(p_values_pro_estado < 0.05, "Significativo", "Não Significativo")

intervalos_confianca_inf_pro_estado <- c(0.04650524, -0.04135497, 0.1126973, 0.1491288, 0.1491288)
intervalos_confianca_sup_pro_estado <- c(0.15106980, 0.07286565, 0.2270221, 0.2803287, 0.2803287)

# Criar um data frame para Pró Estado / Pró Mercado
dados_pro_estado <- data.frame(
  Periodo = factor(periodos_pro_estado, levels = periodos_pro_estado),
  Diferenca_Medias = diferencas_medias_pro_estado,
  P_Value = p_values_pro_estado,
  Significância = significancia_pro_estado,
  Intervalo_Confianca_Inferior = intervalos_confianca_inf_pro_estado,
  Intervalo_Confianca_Superior = intervalos_confianca_sup_pro_estado,
  Dimensao = "Pró Mercado"
)

# Combinar os dois data frames
dados_combinados <- rbind(dados_liberal, dados_pro_estado)

# Criar o gráfico com facet_wrap
grafico_combinado <- ggplot(dados_combinados, aes(x = Periodo, y = Diferenca_Medias, fill = Significância)) +
  geom_errorbar(aes(ymin = Intervalo_Confianca_Inferior, ymax = Intervalo_Confianca_Superior),
                width = 0.4, position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange",
               position = position_dodge(width = 0.9), color = "black", fill = "white") +
  labs(y = "Diferença de Médias (Elite - Povo)", x = "Período",
       subtitle = "") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Adiciona a linha vermelha
  facet_wrap(~ Dimensao, scales = "free_y") +
  scale_fill_manual(values = c("gray" = "gray", "Não Significativo" = "gray",
                               "Significativo" = "blue")) +
  theme_minimal()
grafico_combinado

## Figura 10 - grafico combinado ##########

# Exibir o gráfico combinado
print(grafico_combinado)
grafico_combinado + coord_flip()
grafico_combinado<- grafico_combinado + coord_flip()+ theme_bw()
graf1 <- grafico_combinado + labs(subtitle = "Todos que responderam às questões",
                                  title = "A")+
  ylim(-0.5, 0.34)+theme(
    plot.title = element_text(hjust = 0.5),     # Centralizar o título horizontalmente
    plot.subtitle = element_text(hjust = 0.5)  # Centralizar o subtítulo horizontalmente
  )
graf2 <- grafico_salvar + labs(subtitle = "Somente aqueles pertencentes à partidos com 20 apoiadores e 3 deputados (os selecionados)",
                               title = "B")+
  ylim(-0.5, 0.34)+theme(
    plot.title = element_text(hjust = 0.5),     # Centralizar o título horizontalmente
    plot.subtitle = element_text(hjust = 0.5)  # Centralizar o subtítulo horizontalmente
  )

graf1
graf2

library(gridExtra)
grid.arrange(graf1,graf2)
