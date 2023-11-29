# regressões (distribuni)

# grafico das distribuições comparadas
#bases na pasta distribuni
#recod
library(readr)
df_t1 <- read_csv("df_t1.csv")
df_t2 <- read_csv("df_t2.csv")
df_t3 <- read_csv("df_t3.csv")
df_t5 <- read_csv("df_t5.csv")
#df_t4 <- read_csv("df_t4.csv") Aqui nao
library(tidyverse)
df <- full_join(df_t1,df_t2)
df <- full_join(df,df_t3)
#df <- full_join(df,df_t4) Aqui nao
df <- full_join(df,df_t5)
summary(df)
table(df$base)
table(df$ano)
table(df$partido)
table(df$ano,df$partido)
df$ano <- as.factor(df$ano)
levels(df$ano)
# Defina a ordem desejada
ordem_desejada <- c("2005-2006", "2014-2014", "2017-2018", "2021-2019")

# Reordene a variável df$ano de acordo com a ordem desejada
df$ano <- factor(df$ano, levels = ordem_desejada)
levels(df$ano)
table(df$ano)

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
df_t1$partido  <- as.factor(df_t1$partido )
df_t2$partido  <- as.factor(df_t2$partido )
df_t3$partido  <- as.factor(df_t3$partido )
df_t5$partido  <- as.factor(df_t5$partido )

df$partido <- relevel(df$partido, ref = "PT")
df_t1$partido <- relevel(df_t1$partido, ref = "PT")
df_t2$partido <- relevel(df_t2$partido, ref = "PT")
df_t3$partido <- relevel(df_t3$partido, ref = "PT")
df_t5$partido <- relevel(df_t5$partido, ref = "PT")

# liberal fundamentalismo
fund1<- lm(microGap_fundamentalismo~partido,data=df_t1)
fund2<- lm(microGap_fundamentalismo~partido,data=df_t2)
fund3<- lm(microGap_fundamentalismo~partido,data=df_t3)
fund4<- lm(microGap_fundamentalismo~partido,data=df_t5)
modelao_fund<- lm(microGap_fundamentalismo~ano+partido,data=df)
summary(modelao_fund)
library(sjPlot)
tab_model(fund1,fund2,fund3,fund4,modelao_fund, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
            wrap.labels = 60, p.style = "stars")  

library(marginaleffects)
a1<-plot_cap(modelao_fund, condition=c("partido"), conf_level = .9) + theme_bw()+labs(y="Micro Gap Fundamentalismo (valores preditos)",
                                                                 x="partido")
a2<-plot_cap(modelao_fund, condition=c("ano"), conf_level = .9) + theme_bw()+labs(y="Micro Gap Fundamentalismo (valores preditos)",
                                                                                  x="Período")
library(gridExtra)
grid.arrange(a1,a2,ncol=2)


#Econ

mercas1<- lm(microGap_proMercado~partido,data=df_t1)
mercas2<- lm(microGap_proMercado~partido,data=df_t2)
mercas3<- lm(microGap_proMercado~partido,data=df_t3)
mercas4<- lm(microGap_proMercado~partido,data=df_t5)
modelao_mercas<- lm(microGap_proMercado~ano+partido,data=df)
summary(modelao_mercas)

tab_model(mercas1,mercas2,mercas3,mercas4,modelao_mercas, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")  


a12<-plot_cap(modelao_mercas, condition=c("partido"), conf_level = .9) + theme_bw()+labs(y="Micro Gap proMercado (valores preditos)",
                                                                                         x="partido")
a22<-plot_cap(modelao_mercas, condition=c("ano"), conf_level = .9) + theme_bw()+labs(y="Micro Gap proMercado (valores preditos)",
                                                                                     x="Período")

grid.arrange(a12,a22,ncol=2)

