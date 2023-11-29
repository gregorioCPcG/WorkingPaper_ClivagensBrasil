rm(list=ls())
library(ggplot2)

library(readxl)
df <- read_excel("graficaomacrogapporpartido.xlsx")

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

graf <- ggplot(df, aes(var2, var3, group=Dimensão))+
  geom_line(aes(colour = Dimensão)) + geom_point(aes(colour= Dimensão)) +
  facet_wrap(~ var1)+ 
  theme(legend.position = "bottom")+
  labs(y= "MacroGap", x="Período") + theme_bw()
graf



#

library(dplyr)

# Remover casos em que df$Dimensão == "Favorável" ou df$Dimensão == "Contrário Aborto (solo)"
df_filtrado <- df %>%
  filter(Dimensão != "Favorável / Contrário Aborto (solo)")

# Criar o gráfico com os dados filtrados
graf <- ggplot(df_filtrado, aes(var2, var3, group=Dimensão)) +
  geom_line(aes(colour = Dimensão),size=2) +
  geom_point(aes(colour = Dimensão),size=3) +
  facet_wrap(~ var1) +
  labs(y = "MacroGap", x = "Período") +
  theme_bw() +
  theme(legend.position = "bottom")

# Exibir o gráfico
graf

library(ggdist)
library(tidyquant)
graf+scale_fill_tq()+
  theme_tq()


#
macrao <- read_excel("macrao.xlsx")
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

# Exibir o gráfico
graf + theme_bw()

