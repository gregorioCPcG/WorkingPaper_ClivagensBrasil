#Criar bases de dados do WVS
rm(list=ls())
library(haven)
mergao <- read_dta("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/WVS_TimeSeries_4_0.dta")

#OBS peguei na pasta que ta no meu computador para baixar:
#WVS time-series (1981-2022): Inglehart, R., Haerpfer, C., Moreno, A., Welzel, C., Kizilova, K., Diez-Medrano J., M. Lagos, P. Norris, E. Ponarin & B. Puranen (eds.). 2022. World Values Survey: All Rounds – Country-Pooled Datafile Version 3.0. Madrid, Spain & Vienna, Austria: JD Systems Institute & WVSA Secretariat. doi:10.14281/18241.17
library(dplyr)
df5 <- mergao %>%
  filter(S002VS == 5 & S003 == 76)
df6 <- mergao %>%
  filter(S002VS == 6 & S003 == 76)
df7 <- mergao %>%
  filter(S002VS == 7 & S003 == 76)


print(df7$E036)#valores positivos proESTADO
print(df7$F118)#liberal (valores positivos)CasGay
print(df7$F120)#liberal (valores positivos)Aborto
print(df7$F028)#valores positivos pouca religiosidade
print(df7$F121)#valores positivos liberal (divorce)

df5<- subset(df5, select=c(E179WVS,E036,F118,F120,F028,F121))
write.csv(df5,"df5.csv")

df6<- subset(df6, select=c(E179WVS,E036,F118,F120,F028,F121))
write.csv(df6,"df6.csv")

df7<- subset(df7, select=c(E179WVS,E036,F118,F120,F028,F121))
write.csv(df7,"df7.csv")
