#2017-2018
library(dplyr)
library(ggplot2)
library(gridExtra)
library(readr)
library(labelled)
rm(list=ls())
library(psych)
library(lavaan)
library(semPlot)
povo <- read_csv("df7.csv")
library(haven)
povo$E179WVS[is.na(povo$E179WVS)] <- -55
table(povo$E179WVS)# abaixo os com mais de 20 , falta ver se tem pelo menos 3 deputados
#   -2: 245 entradas
#   -1: 784 entradas
#    5: 174 entradas
#76001: Brazilian Democratic Movement - 28 entradas
#76002: Workers' Party (Partido dos Trabalhadores) - 370 entradas
#76003: Brazilian Social Democracy Party (Partido da Social Democracia Brasileira) - 64 entradas
#76004: Democratic Labour Party (Partido Democrático Trabalhista) - 19 entradas
#76005: Progressive Party / Brazilian Progressive Party (PPB) (Partido Progressista) - 8 entradas
#76009: Communist Party of Brazil (Partido Comunista do Brasil) - 8 entradas
#76015: Social Christian Party (Partido Social Cristão) - 16 entradas
#76026: We can (Podemos) - 5 entradas
#76028: Sustainability Network (Rede Sustentabilidade) - 20 entradas
#76029: Socialism and Liberty Party (Partido Socialismo e Liberdade) - 21 entradas


# Supondo que df seja o seu DataFrame
povo$family <- factor(
  povo$E179WVS,
  levels = c(76001, 76002, 76003, 76004, 76005, 76015, 76026, 76009, 76028, 76029),
  labels = c("Social Democrata", "Social Democrata", "Social Democrata", "Social Democrata",
             "Nacionalista/Democ.Cristão/Liberal", "Nacionalista/Democ.Cristão/Liberal", "Nacionalista/Democ.Cristão/Liberal",
             "Comunista/Ecológico", "Comunista/Ecológico", "Comunista/Ecológico")
)


table(povo$family)


load("BLS8_Data.RData")
df <- bls[bls$wave == 2017, ]
rm(bls)
table(df$wave)

table(df$party_survey)

#   10	PRB=REPUBLICANOS - 3
#   11	PDS-PPR-PPB-PP2003 - 10
#   12	PDT - 7
#   13	PT - 18
#   14	PTB - 6
#   15	PMDB=MDB - 16
#   18	REDE - 2
#   19	PTN=PODEMOS - 2
#   20	PSC - 3
#   22	PL-PRONA-PR-PL - 7
#   23	PCB-PPS-CIDADANIA - 6
#   25	PFL=DEM - 9
#   31	PHS - 1
#   40	PSB - 8
#   43	PV - 2
#   45	PSDB - 22
#   50	PSOL - 4
#   55	PSD - 10
#   65	PCdoB - 3
#   77	SDD=SOLIDARIEDADE - 3
#   90	PROS - 1
# Supondo que df seja o seu DataFrame
df$family <- factor(df$party_survey,
                    levels = c(10, 11, 14, 20, 22, 25, 55, 12, 13, 15, 40, 45, 77, 18, 43, 50, 65,19,23),
                    labels = c("Nacionalista/Democ.Cristão/Liberal", "Nacionalista/Democ.Cristão/Liberal", "Nacionalista/Democ.Cristão/Liberal", "Nacionalista/Democ.Cristão/Liberal",
                               "Nacionalista/Democ.Cristão/Liberal", "Nacionalista/Democ.Cristão/Liberal", "Nacionalista/Democ.Cristão/Liberal",
                               "Social Democrata", "Social Democrata", "Social Democrata", "Social Democrata", "Social Democrata", "Social Democrata",
                               "Comunista/Ecológico", "Comunista/Ecológico", "Comunista/Ecológico", "Comunista/Ecológico",
                               "Nacionalista/Democ.Cristão/Liberal","Social Democrata"),
                    exclude = NULL
)

table(df$family)

#arrumar povo primeiro 
#econ
summary(povo$E036)#remover NAS depois
povo$E036[povo$E036<0]<-NA
-1*povo$E036 -> povo$proMercado
table(povo$proMercado, useNA = "always")
povo$proMercado <- scales::rescale(povo$proMercado, to = c(0, 1))
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

df <- subset(df, select=c(casament_invertido,aborto,proMercado,family))

df->dffull
summary(dffull)

complete5 <- function(...) {
  study <- dffull %>%
    select(...)
  
  return(study[complete.cases(study),])
}

df <- complete5(casament_invertido,aborto,proMercado,family)#como nao tem NA no proMercado
summary(df)


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
table(df$family)
df <- subset(df, select=c(family,proMercado,fundamentalismo))
df$family <- as.factor(df$family)
df$fundamentalismo <- scales::rescale(df$fundamentalismo, to = c(0, 1))
summary(df)
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


