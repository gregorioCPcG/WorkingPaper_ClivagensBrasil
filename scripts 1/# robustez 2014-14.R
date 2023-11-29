# robustez 2024-14

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

df_merca <- subset(df, select=c(proMercado,partido))
povo_merca <- subset(povo, select=c(proMercado,partido))
df_mercaf <- df_merca
povo_mercaf <- povo_merca
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


#

#aborto
str(povo$F120)#liberal (valores positivos)Aborto - inverter
print(dffull$VAL6)#aborto
table(df$VAL6)# 10 é favor - inverter
povo$F120[povo$F120<0]<-NA
table(df$VAL6, useNA = "always")
table(povo$F120, useNA = "always")
df$VAL6_invertido <- -1*df$VAL6
df$VAL6_invertido <- scales::rescale(df$VAL6_invertido, to = c(0, 1))
povo$F120_invertido <- -1*povo$F120
povo$F120_invertido <- scales::rescale(povo$F120_invertido, to = c(0, 1))
df_aborto <- subset(df, select=c(VAL6_invertido,partido))
povo_aborto <- subset(povo, select=c(F120_invertido,partido))
summary(df_aborto)
summary(povo_aborto)

df_abortof <- df_aborto
povo_aborto -> povo_abortof

complete6 <- function(...) {
  study <- df_abortof %>%
    select(...)
  
  return(study[complete.cases(study),])
}

df_aborto <- complete6(VAL6_invertido,partido)
summary(df_aborto)

complete62 <- function(...) {
  study <- povo_abortof %>%
    select(...)
  
  return(study[complete.cases(study),])
}

povo_aborto <- complete62(F120_invertido,partido)
summary(povo_aborto)


# Lista dos objetos que você deseja manter
objetos_a_manter <- c("df_aborto", "df_abortof", "df_merca", "df_mercaf",
                      "povo_aborto", "povo_abortof", "povo_merca",
                      "povo_mercaf")

# Remove todos os objetos, exceto os objetos que você deseja manter
rm(list = setdiff(ls(), objetos_a_manter))

df_aborto$contraaborto <- df_aborto$VAL6_invertido
povo_aborto$contraaborto <- povo_aborto$F120_invertido

df_aborto <- subset(df_aborto, select=c(contraaborto,partido))
povo_aborto <- subset(povo_aborto, select=c(contraaborto,partido))

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
merge$Período <- "2014-2014"
summary(merge)
write.csv(merge,
          "C:/Users/grego/OneDrive/Desktop/work/Congruencia so brasil/graficos oscilacao no tempo/mer14_14.csv")
