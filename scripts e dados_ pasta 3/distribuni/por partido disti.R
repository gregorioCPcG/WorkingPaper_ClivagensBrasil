# grafico das distribuições comparadas

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

library(tidyquant)
library(ggdist)



# PT
sohPT <- df %>% filter(partido == "PT")
droplevels(sohPT) -> sohPT

# Crie o histograma com densidade ou percentagem para a dimensão econômica
sohPT_proMercado <- ggplot(sohPT, aes(x = proMercado, y = ..density..)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(
    subtitle = "PT dimensão econômica",
    y = "Densidade",
    fill = "Ano"
  ) +
  facet_grid(ano ~ base) +
  theme_minimal()

# Imprima o histograma
print(sohPT_proMercado)

# Crie o histograma com densidade ou percentagem para a dimensão cultural
sohPT_fundamentalismo <- ggplot(sohPT, aes(x = fundamentalismo, y = ..density..)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(
    subtitle = "PT dimensão cultural",
    y = "Densidade",
    fill = "Ano"
  ) +
  facet_grid(ano ~ base) +
  theme_minimal()

# Imprima o histograma
print(sohPT_fundamentalismo)
levels(df$partido)
# MDB/PMDB
sohMDB <- df %>% filter(partido == "MDB/PMDB")
droplevels(sohMDB) -> sohMDB

# Crie o histograma com densidade ou percentagem para a dimensão econômica
sohMDB_proMercado <- ggplot(sohMDB, aes(x = proMercado, y = ..density..)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(
    subtitle = "MDB/PMDB dimensão econômica",
    y = "Densidade",
    fill = "Ano"
  ) +
  facet_grid(ano ~ base) +
  theme_minimal()

# Imprima o histograma
print(sohMDB_proMercado)

# Crie o histograma com densidade ou percentagem para a dimensão cultural
sohMDB_fundamentalismo <- ggplot(sohMDB, aes(x = fundamentalismo, y = ..density..)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(
    subtitle = "MDB/PMDB dimensão cultural",
    y = "Densidade",
    fill = "Ano"
  ) +
  facet_grid(ano ~ base) +
  theme_minimal()

# Imprima o histograma
print(sohMDB_fundamentalismo)


# PSDB
sohPSDB <- df %>% filter(partido == "PSDB")
droplevels(sohPSDB) -> sohPSDB

# Crie o histograma com densidade ou percentagem para a dimensão econômica
sohPSDB_proMercado <- ggplot(sohPSDB, aes(x = proMercado, y = ..density..)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(
    subtitle = "PSDB dimensão econômica",
    y = "Densidade",
    fill = "Ano"
  ) +
  facet_grid(ano ~ base) +
  theme_minimal()

# Imprima o histograma
print(sohPSDB_proMercado)

# Crie o histograma com densidade ou percentagem para a dimensão cultural
sohPSDB_fundamentalismo <- ggplot(sohPSDB, aes(x = fundamentalismo, y = ..density..)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(
    subtitle = "PSDB dimensão cultural",
    y = "Densidade",
    fill = "Ano"
  ) +
  facet_grid(ano ~ base) +
  theme_minimal()

# Imprima o histograma
print(sohPSDB_fundamentalismo)
# PSL
sohPSL <- df %>% filter(partido == "PSL")
droplevels(sohPSL) -> sohPSL

# Crie o histograma com densidade ou percentagem para a dimensão econômica
sohPSL_proMercado <- ggplot(sohPSL, aes(x = proMercado, y = ..density..)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(
    subtitle = "PSL dimensão econômica",
    y = "Densidade",
    fill = "Ano"
  ) +
  facet_grid(ano ~ base) +
  theme_minimal()

# Imprima o histograma
print(sohPSL_proMercado)

# Crie o histograma com densidade ou percentagem para a dimensão cultural
sohPSL_fundamentalismo <- ggplot(sohPSL, aes(x = fundamentalismo, y = ..density..)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(
    subtitle = "PSL dimensão cultural",
    y = "Densidade",
    fill = "Ano"
  ) +
  facet_grid(ano ~ base) +
  theme_minimal()

# Imprima o histograma
print(sohPSL_fundamentalismo)

# PDT
sohPDT <- df %>% filter(partido == "PDT")
droplevels(sohPDT) -> sohPDT

# Crie o histograma com densidade ou percentagem para a dimensão econômica
sohPDT_proMercado <- ggplot(sohPDT, aes(x = proMercado, y = ..density..)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(
    subtitle = "PDT dimensão econômica",
    y = "Densidade",
    fill = "Ano"
  ) +
  facet_grid(ano ~ base) +
  theme_minimal()

# Imprima o histograma
print(sohPDT_proMercado)

# Crie o histograma com densidade ou percentagem para a dimensão cultural
sohPDT_fundamentalismo <- ggplot(sohPDT, aes(x = fundamentalismo, y = ..density..)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(
    subtitle = "PDT dimensão cultural",
    y = "Densidade",
    fill = "Ano"
  ) +
  facet_grid(ano ~ base) +
  theme_minimal()

# Imprima o histograma
print(sohPDT_fundamentalismo)
# PSB
sohPSB <- df %>% filter(partido == "PSB")
droplevels(sohPSB) -> sohPSB

# Crie o histograma com densidade ou percentagem para a dimensão econômica
sohPSB_proMercado <- ggplot(sohPSB, aes(x = proMercado, y = ..density..)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(
    subtitle = "PSB dimensão econômica",
    y = "Densidade",
    fill = "Ano"
  ) +
  facet_grid(ano ~ base) +
  theme_minimal()

# Imprima o histograma
print(sohPSB_proMercado)

# Crie o histograma com densidade ou percentagem para a dimensão cultural
sohPSB_fundamentalismo <- ggplot(sohPSB, aes(x = fundamentalismo, y = ..density..)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(
    subtitle = "PSB dimensão cultural",
    y = "Densidade",
    fill = "Ano"
  ) +
  facet_grid(ano ~ base) +
  theme_minimal()

# Imprima o histograma
print(sohPSB_fundamentalismo)

# PFL
sohPFL <- df %>% filter(partido == "PFL")
droplevels(sohPFL) -> sohPFL

# Crie o histograma com densidade ou percentagem para a dimensão econômica
sohPFL_proMercado <- ggplot(sohPFL, aes(x = proMercado, y = ..density..)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(
    subtitle = "PFL dimensão econômica",
    y = "Densidade",
    fill = "Ano"
  ) +
  facet_grid(ano ~ base) +
  theme_minimal()

# Imprima o histograma
print(sohPFL_proMercado)

# Crie o histograma com densidade ou percentagem para a dimensão cultural
sohPFL_fundamentalismo <- ggplot(sohPFL, aes(x = fundamentalismo, y = ..density..)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(
    subtitle = "PFL dimensão cultural",
    y = "Densidade",
    fill = "Ano"
  ) +
  facet_grid(ano ~ base) +
  theme_minimal()

# Imprima o histograma
print(sohPFL_fundamentalismo)

# PSOL
sohPSOL <- df %>% filter(partido == "PSOL")
droplevels(sohPSOL) -> sohPSOL

# Crie o histograma com densidade ou percentagem para a dimensão econômica
sohPSOL_proMercado <- ggplot(sohPSOL, aes(x = proMercado, y = ..density..)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(
    subtitle = "PSOL dimensão econômica",
    y = "Densidade",
    fill = "Ano"
  ) +
  facet_grid(ano ~ base) +
  theme_minimal()

# Imprima o histograma
print(sohPSOL_proMercado)

# Crie o histograma com densidade ou percentagem para a dimensão cultural
sohPSOL_fundamentalismo <- ggplot(sohPSOL, aes(x = fundamentalismo, y = ..density..)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(
    subtitle = "PSOL dimensão cultural",
    y = "Densidade",
    fill = "Ano"
  ) +
  facet_grid(ano ~ base) +
  theme_minimal()

# Imprima o histograma
print(sohPSOL_fundamentalismo)

# PTB
sohPTB <- df %>% filter(partido == "PTB")
droplevels(sohPTB) -> sohPTB

# Crie o histograma com densidade ou percentagem para a dimensão econômica
sohPTB_proMercado <- ggplot(sohPTB, aes(x = proMercado, y = ..density..)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(
    subtitle = "PTB dimensão econômica",
    y = "Densidade",
    fill = "Ano"
  ) +
  facet_grid(ano ~ base) +
  theme_minimal()

# Imprima o histograma
print(sohPTB_proMercado)

# Crie o histograma com densidade ou percentagem para a dimensão cultural
sohPTB_fundamentalismo <- ggplot(sohPTB, aes(x = fundamentalismo, y = ..density..)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(
    subtitle = "PTB dimensão cultural",
    y = "Densidade",
    fill = "Ano"
  ) +
  facet_grid(ano ~ base) +
  theme_minimal()

# Imprima o histograma
print(sohPTB_fundamentalismo)

