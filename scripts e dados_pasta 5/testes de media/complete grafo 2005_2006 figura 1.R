#2005-2006
library(dplyr)
library(ggplot2)
library(gridExtra)
library(readr)
library(labelled)
library(psych)
library(lavaan)
library(semTools)
rm(list=ls())
povo <- read_csv("df5.csv")
povo$E179WVS[is.na(povo$E179WVS)] <- -55
table(povo$E179WVS)# abaixo os com mais de 20 , falta ver se tem pelo menos 3 deputados
#partidos WVS
#76001	BRA:Brazilian Democratic Movement = + q 20
#76002	BRA:Workers´ Party = + q 20
#76003	BRA:Brazilian Social Democracy Party = + q 20
#76004	BRA:Democratic Labour Party = + q 20
#76006	BRA:Liberal Front / Democrats = + q 20
#76007	BRA:Brazilian Labour Party = + q 20
#76010  BRA: Green Party -= + q 20 - NÃO TEM DEPUTADOS SUFICIE
#76009	BRA:Communist Party of Brazil = + q 20 - MAS NÃO TEM DEPUTADOS SUFICIE
#76029	BRA:Socialism and Liberty Party = + q 20 - MAS NÃO TEM DEPUTADOS SUFICIE
library(haven)
dffull <-read_sav("BASEDATOS_BRASIL_55.sav")
df <- remove_labels(dffull)
print(dffull$partido)
table(df$partido)#abaixo mais q três
#1    PT = + q 3 
#2  PMDB = + q 3
#3   PFL = + q 3
#4    PP = + q 3- MAS NÃO TEM POVO SUFICIE
#5   PTB = + q 3
#6  PSDB = + q 3
#7    PL = + q 3 - MAS NÃO TEM POVO SUFICIE
#8   PPS = + q 3 - MAS NÃO TEM POVO SUFICIE
#9   PSB = + q 3 - MAS NÃO TEM POVO SUFICIE
#10   PDT = + q 3

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
# Recodificação recodifiquei 1 e 2 como 1; 3 e 4 como 2; 5 e 6 como 3; 7 e 8 como 4; e 9 e 10 como 5
str(povo$E036)
povo$E036 <- cut(povo$E036, breaks = c(0, 2, 4, 6, 8, 10), labels = c(1, 2, 3, 4, 5), include.lowest = TRUE)
table(povo$E036)
#OBS
#Obs no arquivo criar base
#print(df7$E036)#valores positivos proESTADO - inverter!
povo$E036 <- as.numeric(povo$E036)
table(povo$E036, useNA = "always")
summary(povo$E036)
-1*povo$E036 -> povo$proMercado
print(dffull$p28)#nao tem missing
df$proMercado <- df$p28
df$proMercado <- scales::rescale(df$proMercado, to = c(0, 1))
povo$proMercado <- scales::rescale(povo$proMercado, to = c(0, 1))
table(df$proMercado, useNA = "always")
table(povo$proMercado, useNA = "always")
summary(df$proMercado)
summary(povo$proMercado)
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


df <- subset(df, select=c(p60b,p64,p65,proMercado))#remover NAS
summary(df)
df->dffull
complete5 <- function(...) {
  study <- dffull %>%
    select(...)
  
  return(study[complete.cases(study),])
}

df <- complete5(p60b,p64,p65,proMercado)#como nao tem NA no proMercado
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
df <- subset(df, select=c(proMercado,fundamentalismo))
#df$partido <- as.factor(df$partido)
df$fundamentalismo <- scales::rescale(df$fundamentalismo, to = c(0, 1))
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

