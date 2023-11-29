rm(list=ls())
library(ggplot2)
library(readr)
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
library(dplyr)
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



a <- merge %>%
  dplyr::filter(Dimensão == "Liberal / Fundamentalismo adaptado") %>%
  ggplot(aes(x=microGap, y=Período, fill=Período))+ggdist::stat_halfeye(adjust=0.5,
                                                                           justification = -.2,
                                                                           .width=0,
                                                                           point_colour=NA) +
  geom_boxplot(width=-.12,outlier.colour = NA,alpha=0.5)+
  #ggdist::stat_dots(side="left",justification=1.1, binwidth=.25) +
  scale_fill_tq()+ facet_wrap(~ partido)+
  theme_tq() +
  labs(title= "Micro Gap por partido - Liberal/Fundamentalismo adaptado",
       subtitle = "Qual partido a nível individual representa o eleitor?",
       x = "Micro Gap",
       y= "Período",
       fill= "Período",
  ) + theme(legend.position = "right")
a


b <- merge %>%
  dplyr::filter(Dimensão == "Pró Aborto V.S. Contra Aborto") %>%
  ggplot(aes(x=microGap, y=Período, fill=Período))+ggdist::stat_halfeye(adjust=0.5,
                                                                        justification = -.2,
                                                                        .width=0,
                                                                        point_colour=NA) +
  geom_boxplot(width=-.12,outlier.colour = NA,alpha=0.5)+
  #ggdist::stat_dots(side="left",justification=1.1, binwidth=.25) +
  scale_fill_tq()+ facet_wrap(~ partido)+
  theme_tq() +
  labs(title= "Micro Gap por partido - Opinião sobre Aborto",
       subtitle = "Qual partido a nível individual representa o eleitor?",
       x = "Micro Gap",
       y= "Período",
       fill= "Período",
  ) + theme(legend.position = "right")
b


c <- merge %>%
  dplyr::filter(Dimensão == "Pró Estado V.S Pró Mercado") %>%
  ggplot(aes(x=microGap, y=Período, fill=Período))+ggdist::stat_halfeye(adjust=0.5,
                                                                        justification = -.2,
                                                                        .width=0,
                                                                        point_colour=NA) +
  geom_boxplot(width=-.12,outlier.colour = NA,alpha=0.5)+
  #ggdist::stat_dots(side="left",justification=1.1, binwidth=.25) +
  scale_fill_tq()+ facet_wrap(~ partido)+
  theme_tq() +
  labs(title= "Micro Gap por partido - Opinião sobre questão Pró Estado v.s Pró Mercado",
       subtitle = "Qual partido a nível individual representa o eleitor?",
       x = "Micro Gap",
       y= "Período",
       fill= "Período",
  ) + theme(legend.position = "right")
c


#


a1 <- merge %>%
  dplyr::filter(Dimensão == "Liberal / Fundamentalismo adaptado") %>%
  ggplot(aes(x=microGap, y=Período, fill=Período))+ggdist::stat_halfeye(adjust=0.5,
                                                                        justification = -.2,
                                                                        .width=0,
                                                                        point_colour=NA) +
  geom_boxplot(width=-.12,outlier.colour = NA,alpha=0.5)+
  #ggdist::stat_dots(side="left",justification=1.1, binwidth=.25) +
  scale_fill_tq()+ #facet_wrap(~ partido)+
  theme_tq() +
  labs(title= "Micro Gap - Liberal/Fundamentalismo adaptado",
       #subtitle = "Qual partido a nível individual representa o eleitor?",
       x = "Micro Gap",
       y= "Período",
       fill= "Período",
  ) + theme(legend.position = "right")
a1


b1 <- merge %>%
  dplyr::filter(Dimensão == "Pró Aborto V.S. Contra Aborto") %>%
  ggplot(aes(x=microGap, y=Período, fill=Período))+ggdist::stat_halfeye(adjust=0.5,
                                                                        justification = -.2,
                                                                        .width=0,
                                                                        point_colour=NA) +
  geom_boxplot(width=-.12,outlier.colour = NA,alpha=0.5)+
  #ggdist::stat_dots(side="left",justification=1.1, binwidth=.25) +
  scale_fill_tq()+ #facet_wrap(~ partido)+
  theme_tq() +
  labs(title= "Micro Gap - Opinião sobre Aborto",
       #subtitle = "Qual partido a nível individual representa o eleitor?",
       x = "Micro Gap",
       y= "Período",
       fill= "Período",
  ) + theme(legend.position = "right")
b1


c1 <- merge %>%
  dplyr::filter(Dimensão == "Pró Estado V.S Pró Mercado") %>%
  ggplot(aes(x=microGap, y=Período, fill=Período))+ggdist::stat_halfeye(adjust=0.5,
                                                                        justification = -.2,
                                                                        .width=0,
                                                                        point_colour=NA) +
  geom_boxplot(width=-.12,outlier.colour = NA,alpha=0.5)+
  #ggdist::stat_dots(side="left",justification=1.1, binwidth=.25) +
  scale_fill_tq()+ #facet_wrap(~ partido)+
  theme_tq() +
  labs(title= "Micro Gap - Opinião sobre questão Pró Estado v.s Pró Mercado",
       #subtitle = "Qual partido a nível individual representa o eleitor?",
       x = "Micro Gap",
       y= "Período",
       fill= "Período",
  ) + theme(legend.position = "right")
c1




d <- merge %>%
  #dplyr::filter(Dimensão == "Pró Estado V.S Pró Mercado") %>%
  ggplot(aes(x=microGap, y=Período, fill=Período))+ggdist::stat_halfeye(adjust=0.5,
                                                                        justification = -.2,
                                                                        .width=0,
                                                                        point_colour=NA) +
  geom_boxplot(width=-.12,outlier.colour = NA,alpha=0.5)+
  #ggdist::stat_dots(side="left",justification=1.1, binwidth=.25) +
  scale_fill_tq()+ facet_wrap(~ Dimensão)+
  theme_tq() +
  labs(title= "Micro Gap por Período",
       #subtitle = "Qual partido a nível individual representa o eleitor?",
       x = "Micro Gap",
       y= "Período",
       fill= "Período",
  ) + theme(legend.position = "right")
d


d2 <- merge %>%
  dplyr::filter(Dimensão != "Pró Aborto V.S. Contra Aborto") %>%
  ggplot(aes(x=microGap, y=Período, fill=Período))+ggdist::stat_halfeye(adjust=0.5,
                                                                        justification = -.2,
                                                                        .width=0,
                                                                        point_colour=NA) +
  geom_boxplot(width=-.12,outlier.colour = NA,alpha=0.5)+
  #ggdist::stat_dots(side="left",justification=1.1, binwidth=.25) +
  scale_fill_tq()+ #facet_wrap(~ Dimensão)+
  theme_tq() +
  labs(title= "Micro Gap Todo o Período",
       subtitle = "Liberal / Fundamentalismo adaptado e Pró Estado v.s Pró Mercado",
       x = "Micro Gap",
       y= "Período",
       fill= "Período",
  ) + theme(legend.position = "right")
d2

d3 <- merge %>%
  dplyr::filter(Dimensão != "Pró Aborto V.S. Contra Aborto") %>%
  dplyr::filter(partido %in% c("MDB/PMDB","PT","PSDB","PSL","PDT")) %>%
  ggplot(aes(x=microGap, y=partido, fill=partido))+ggdist::stat_halfeye(adjust=0.5,
                                                                        justification = -.2,
                                                                        .width=0,
                                                                        point_colour=NA) +
  geom_boxplot(width=-.12,outlier.colour = NA,alpha=0.5)+
  #ggdist::stat_dots(side="left",justification=1.1, binwidth=.25) +
  scale_fill_tq()+ #facet_wrap(~ partido)+
  theme_tq() +
  labs(title= "Micro Gap Todo o Período por Legenda Partidária",
       subtitle = "Liberal / Fundamentalismo adaptado e Pró Estado v.s Pró Mercado",
       x = "Micro Gap",
       y= "",
       fill= "Legenda partidária",
       caption = "OBS: partidos citados em mais de um período",
  ) + theme(legend.position = "right")
d3



#
#Sem PT

#
# Criar uma nova base sem os casos onde o partido é "PT"
merge_semPT <- merge[merge$partido != "PT", ]


a12 <- merge_semPT %>%
  dplyr::filter(Dimensão == "Liberal / Fundamentalismo adaptado") %>%
  ggplot(aes(x=microGap, y=Período, fill=Período))+ggdist::stat_halfeye(adjust=0.5,
                                                                        justification = -.2,
                                                                        .width=0,
                                                                        point_colour=NA) +
  geom_boxplot(width=-.12,outlier.colour = NA,alpha=0.5)+
  #ggdist::stat_dots(side="left",justification=1.1, binwidth=.25) +
  scale_fill_tq()+ #facet_wrap(~ partido)+
  theme_tq() +
  labs(title= "Micro Gap - Liberal/Fundamentalismo adaptado 
       sem o PT",
       #subtitle = "Qual partido a nível individual representa o eleitor?",
       x = "Micro Gap",
       y= "Período",
       fill= "Período",
  ) + theme(legend.position = "right")
a12


b12 <- merge_semPT %>%
  dplyr::filter(Dimensão == "Pró Aborto V.S. Contra Aborto") %>%
  ggplot(aes(x=microGap, y=Período, fill=Período))+ggdist::stat_halfeye(adjust=0.5,
                                                                        justification = -.2,
                                                                        .width=0,
                                                                        point_colour=NA) +
  geom_boxplot(width=-.12,outlier.colour = NA,alpha=0.5)+
  #ggdist::stat_dots(side="left",justification=1.1, binwidth=.25) +
  scale_fill_tq()+ #facet_wrap(~ partido)+
  theme_tq() +
  labs(title= "Micro Gap - Opinião sobre Aborto 
       sem o PT",
       #subtitle = "Qual partido a nível individual representa o eleitor?",
       x = "Micro Gap",
       y= "Período",
       fill= "Período",
  ) + theme(legend.position = "right")
b12


c12 <- merge_semPT %>%
  dplyr::filter(Dimensão == "Pró Estado V.S Pró Mercado") %>%
  ggplot(aes(x=microGap, y=Período, fill=Período))+ggdist::stat_halfeye(adjust=0.5,
                                                                        justification = -.2,
                                                                        .width=0,
                                                                        point_colour=NA) +
  geom_boxplot(width=-.12,outlier.colour = NA,alpha=0.5)+
  #ggdist::stat_dots(side="left",justification=1.1, binwidth=.25) +
  scale_fill_tq()+ #facet_wrap(~ partido)+
  theme_tq() +
  labs(title= "Micro Gap - Opinião sobre questão Pró Estado v.s Pró Mercado
       sem o PT",
       #subtitle = "Qual partido a nível individual representa o eleitor?",
       x = "Micro Gap",
       y= "Período",
       fill= "Período",
  ) + theme(legend.position = "right")
c12


library(gridExtra)
grid.arrange(a1,a12)
grid.arrange(b1,b12)
grid.arrange(c1,c12)

# de fato

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


grid.arrange(defato1, defato2)
#
cores_manual <- c("#000000", "#333333", "#666666", "#999999", "#CCCCCC")

defato1+scale_fill_manual(values = cores_manual)
defato2+scale_fill_manual(values = cores_manual)
