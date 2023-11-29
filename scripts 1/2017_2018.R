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
#histogramas
#proMercado povo

proMercado_tab1 <- ggplot(povo, aes(proMercado)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "Pró Mercado", title = "Público; distribuição")+theme_minimal()
proMercado_tab1#para testar
#proMercado elite
proMercado_tab2 <- ggplot(df, aes(proMercado)) +
  geom_histogram(fill = "red", bins = 30) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "Pró Mercado", title = "Elite; distribuição")+theme_minimal()
proMercado_tab2#para testar

#microGap proMercado
proMercado_tab3 <- ggplot(povo, aes(microGap_proMercado)) +
  geom_histogram(fill = "black", bins = 30) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "Micro Gap Pró Mercado", title = "Micro Gap; distribuição")+theme_minimal()
proMercado_tab3#para testar

# Gráfico de caixa de povo$microGap_proMercado por categoria de povo$partido
proMercado_tab4 <- ggplot(povo, aes(x = partido, y = microGap_proMercado, fill = partido)) +
  geom_boxplot() +
  labs(x = "Partido", y = "microGap_proMercado") +
  scale_fill_discrete(name = "Partido") +
  ggtitle("Micro Gap Visão Pró Mercado por partido; distribuição")
proMercado_tab4#testar
# Gráfico de caixa de df$proMercado por categoria de df$partido
proMercado_tab5 <- ggplot(df, aes(x = partido, y = proMercado, fill = partido)) +
  geom_boxplot() +
  labs(x = "Partido", y = "proMercado") +
  scale_fill_discrete(name = "Partido") +
  ggtitle("Posição das elites Visão Pró Mercado por partido; distribuição")
proMercado_tab5#testar


# Gráfico de caixa de povo$proMercado por categoria de df$partido
proMercado_tab6<- ggplot(povo, aes(x = partido, y = proMercado, fill = partido)) +
  geom_boxplot() +
  labs(x = "Partido", y = "proMercado") +
  scale_fill_discrete(name = "Partido") +
  ggtitle("Posição do público Visão Pró Mercado por partido; distribuição")
proMercado_tab6#testar

# Definir as dimensões da área de plotagem
grid.arrange(proMercado_tab5,proMercado_tab2,proMercado_tab6,proMercado_tab1,proMercado_tab4,
             proMercado_tab3)

#merge tabela 7

df$base <- "Elite" 
povo$base <- "Povo"


merge <- full_join(df,povo)

# Transformar a variável merge$base em um fator
merge$base_factor <- factor(merge$base)

# Criar o gráfico com facet_wrap
proMercado_tab7 <- ggplot(merge, aes(x = partido, y = proMercado, fill = partido)) +
  geom_boxplot() +
  labs(x = "Partido", y = "proMercado") +
  scale_fill_discrete(name = "Partido") +
  ggtitle("Visão Pró Mercado por partido; distribuição") +
  facet_wrap(~ base_factor)  # Adicionando o facet_wrap

proMercado_tab7

#

#fundamentalismo povo

fundamentalismo_tab1 <- ggplot(povo, aes(fundamentalismo)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "Fundamentalismo", title = "Público; distribuição")+theme_minimal()
fundamentalismo_tab1#para testar
#fundamentalismo elite
fundamentalismo_tab2 <- ggplot(df, aes(fundamentalismo)) +
  geom_histogram(fill = "red", bins = 30) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "Fundamentalismo", title = "Elite; distribuição")+theme_minimal()
fundamentalismo_tab2#para testar

#microGap fundamentalismo
fundamentalismo_tab3 <- ggplot(povo, aes(microGap_fundamentalismo)) +
  geom_histogram(fill = "black", bins = 30) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "Micro Gap Fundamentalismo", title = "Micro Gap; distribuição")+theme_minimal()
fundamentalismo_tab3#para testar

# Gráfico de caixa de povo$microGap_fundamentalismo por categoria de povo$partido
fundamentalismo_tab4 <- ggplot(povo, aes(x = partido, y = microGap_fundamentalismo, fill = partido)) +
  geom_boxplot() +
  labs(x = "Partido", y = "microGap_fundamentalismo") +
  scale_fill_discrete(name = "Partido") +
  ggtitle("Micro Gap Liberal / Fundamentalismo por partido; distribuição")
fundamentalismo_tab4#testar
# Gráfico de caixa de df$fundamentalismo por categoria de df$partido
fundamentalismo_tab5 <- ggplot(df, aes(x = partido, y = fundamentalismo, fill = partido)) +
  geom_boxplot() +
  labs(x = "Partido", y = "fundamentalismo") +
  scale_fill_discrete(name = "Partido") +
  ggtitle("Posição das elites Liberal / Fundamentalismo por partido; distribuição")
fundamentalismo_tab5#testar


# Gráfico de caixa de povo$fundamentalismo por categoria de df$partido
fundamentalismo_tab6<- ggplot(povo, aes(x = partido, y = fundamentalismo, fill = partido)) +
  geom_boxplot() +
  labs(x = "Partido", y = "fundamentalismo") +
  scale_fill_discrete(name = "Partido") +
  ggtitle("Posição do público Liberal / Fundamentalismo  por partido; distribuição")
fundamentalismo_tab6#testar

# Definir as dimensões da área de plotagem
grid.arrange(fundamentalismo_tab5,fundamentalismo_tab2,fundamentalismo_tab6,fundamentalismo_tab1,fundamentalismo_tab4,
             fundamentalismo_tab3)



#

#merge tabela 7
# Transformar a variável merge$base em um fator
merge$base_factor <- factor(merge$base)

# Criar o gráfico com facet_wrap
fundamentalismo_tab7 <- ggplot(merge, aes(x = partido, y = fundamentalismo, fill = partido)) +
  geom_boxplot() +
  labs(x = "Partido", y = "fundamentalismo") +
  scale_fill_discrete(name = "Partido") +
  ggtitle("Visão Fundamentalista por partido; distribuição") +
  facet_wrap(~ base_factor)  # Adicionando o facet_wrap

fundamentalismo_tab7


#puxar pra pasta graficos oscilacao
summary(povo)
povo$microGap_fundamentalismo -> povo$microGap
povo_fundamentalismo <- subset(povo, select=c(partido,microGap))
povo_fundamentalismo$Dimensão <- "Liberal / Fundamentalismo adaptado"
povo_fundamentalismo$Período <- "2017-2018"
summary(povo_fundamentalismo)
write.csv(povo_fundamentalismo,
          "C:/Users/grego/OneDrive/Desktop/work/Congruencia so brasil/graficos oscilacao no tempo/fund17_18.csv")
