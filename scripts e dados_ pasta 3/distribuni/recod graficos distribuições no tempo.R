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
library(tidyquant)
library(ggdist)
df_filtrado <- df[df$ano != "Out 2018-2019", ]#remover para ficar mais bunito

library(dplyr)

# Remova as combinações onde o valor de fundamentalismo é NA
df_filtrado <- df_filtrado %>%
  filter(!is.na(fundamentalismo))

# Agora, crie o gráfico com os dados filtrados
df_filtrado %>%
  ggplot(aes(x = base, y = fundamentalismo, fill = base)) +
  geom_boxplot(width = -0.3, outlier.colour = "red", outlier.shape = 1, alpha = 0.5, coef = 1.5) +
  stat_summary(fun = "mean", geom = "point", size = 3, shape = 21, fill = "white", colour = "black") +
  scale_fill_tq() +
  scale_colour_tq() +
  scale_y_continuous(breaks = seq(-1, 1, 0.2)) +
  theme_bw() +
  facet_wrap(partido ~ .) +
  facet_grid(ano ~ partido) +
  scale_fill_manual(values = c("green", "purple")) +
  labs(
    title = "",
    subtitle = "",
    x = "",
    y = "Liberal / Fundamentalismo adaptado",
    fill = "",
    colour = "Outliers"
  ) +
  coord_flip() +
  labs(caption = "OBS: O círculo branco aponta a média") +
  theme(plot.caption = element_text(size = 9))

#econ

df_filtrado %>%
  ggplot(aes(x = base, y = proMercado, fill = base)) +
  geom_boxplot(width = -0.3, outlier.colour = "red", outlier.shape = 1, alpha = 0.5, coef = 1.5) +
  stat_summary(fun = "mean", geom = "point", size = 3, shape = 21, fill = "white", colour = "black") +
  scale_fill_tq() +
  scale_colour_tq() +
  scale_y_continuous(breaks = seq(-1, 1, 0.2)) +
  theme_bw() +
  facet_wrap(partido ~ .) +
  facet_grid(ano ~ partido) +
  scale_fill_manual(values = c("green", "purple")) +
  labs(
    title = "",
    subtitle = "",
    x = "",
    y = "Pró Estado / Pró Mercado",
    fill = "",
    colour = "Outliers"
  ) +
  coord_flip() +
  labs(caption = "OBS: O círculo branco aponta a média") +
  theme(plot.caption = element_text(size = 9))
#
# micros gaps

df_filtrado %>%
  ggplot(aes(x = base, y = microGap_proMercado)) +
  geom_boxplot(width = -0.3, outlier.colour = "red", outlier.shape = 1, alpha = 0.5, coef = 1.5) +
  stat_summary(fun = "mean", geom = "point", size = 3, shape = 21, fill = "white", colour = "black") +
  scale_fill_tq() +
  scale_y_continuous(breaks = seq(-1, 1, 0.2)) +
  scale_fill_tq() +
  facet_wrap(partido ~ .) +
  facet_grid(ano ~ partido) +
  scale_fill_manual(values = c("green", "purple")) +
  labs(
    title = "",
    subtitle = "",
    x = "",
    y = "Pró Estado / Pró Mercado",
    fill = "",
    colour = "Outliers"
  ) +
  coord_flip() +
  labs(caption = "OBS: O círculo branco aponta a média") +
  theme(plot.caption = element_text(size = 9))



library(ggplot2)
# Reordenar os níveis da variável partido da esquerda para a direita
df_filtrado$partido <- factor(df_filtrado$partido, levels = c("PSOL", "PT", "PDT", "PSB",
                                                              "MDB/PMDB", "PSDB", "PFL",
                                                              "PSL", "PTB"))


# Se df_filtrado for o seu dataframe
# Carregue a biblioteca ggplot2
library(ggplot2)

# Se df_filtrado for o seu dataframe
ggplot(df_filtrado, aes(x = microGap_proMercado, y = microGap_fundamentalismo, shape = ano, color = partido)) +
  geom_point() +
  facet_wrap(ano ~ .) +
  labs(
    title = "Gráfico de Dispersão com Três Variáveis Categóricas e Duas Numéricas",
    x = "Micro Gap Pró Mercado",
    y = "Micro Gap Fundamentalismo",
    color = "Partido",
    shape = "Ano"
  ) +
  scale_color_brewer(palette = "RdBu") +  # Escolhe a paleta de cores RdBu
  theme(
    plot.title = element_text(size = 14),
    legend.title = element_blank(),
    legend.position = "right"
  )


library(ggplot2)
library(RColorBrewer)

# Defina uma paleta de cores que vai do vermelho ao azul
colors <- brewer.pal(n = 9, name = "RdBu")

# Se df_filtrado for o seu dataframe
ggplot(df_filtrado, aes(x = microGap_proMercado, y = microGap_fundamentalismo, shape = ano, color = partido)) +
  geom_point() +
  facet_wrap(ano ~ .) +
  labs(
    title = "Gráfico de Dispersão com Três Variáveis Categóricas e Duas Numéricas",
    x = "Micro Gap Pró Mercado",
    y = "Micro Gap Fundamentalismo",
    color = "Partido",
    shape = "Ano"
  ) +
  scale_color_manual(values = colors) +  # Use a paleta de cores definida
  theme(
    plot.title = element_text(size = 14),
    legend.title = element_blank(),
    legend.position = "right"
  )


#

ggplot(df_filtrado, aes(x = microGap_proMercado, y = microGap_fundamentalismo, color = ano)) +
  geom_point() +
  facet_wrap(ano ~ .) +
  labs(
    title = "Gráfico de Dispersão com Três Variáveis Categóricas e Duas Numéricas",
    x = "Micro Gap Pró Estado /Pró Mercado",
    y = "Micro Gap Liberal / Fundamentalismo",
    color = "Partido",
    shape = "Ano"
  ) +
  scale_color_manual(values = colors) +  # Use a paleta de cores definida
  theme(
    plot.title = element_text(size = 14),
    legend.title = element_blank(),
    legend.position = "right"
  )

#

# por partido

f44<-ggplot(df_filtrado, aes(x = partido, y = microGap_fundamentalismo, fill = partido)) +
  geom_boxplot() + coord_flip()+
  facet_wrap(ano ~ .) +
  labs(
    subtitle = "",
    x = "",
    y = "micro gap Liberal / Fundamentalismo",
    color = "Partido",
    shape = "Ano"
  ) +
  scale_fill_manual(values = colors) +  # Use a paleta de cores definida
  scale_fill_manual(values = colors)+
  theme(
    plot.title = element_text(size = 14),
    legend.title = element_blank(),
    legend.position = "right"
  )+ theme_minimal()+
  stat_summary(fun = "mean", geom = "point", size = 3, shape = 21, fill = "white", colour = "black") 

f44
f45<-ggplot(df_filtrado, aes(x = partido, y = microGap_proMercado, fill = partido)) +
  geom_boxplot() + coord_flip()+
  facet_wrap(ano ~ .) +
  labs(
    subtitle = "",
    x = "",
    y = "micro gap Pró Mercado / Pró Estado",
    color = "Partido",
    shape = "Ano"
  ) +
  scale_fill_manual(values = colors) +  # Use a paleta de cores definida
  theme(
    plot.title = element_text(size = 14),
    legend.title = element_blank(),
    legend.position = "none"
  ) + theme_minimal() +
  stat_summary(fun = "mean", geom = "point", size = 3, shape = 21, fill = "white", colour = "black") 
f45
library(gridExtra)
grid.arrange(f44,f45)


# Carregue a biblioteca tidyverse
# Carregue a biblioteca tidyverse
library(tidyverse)

# Renomeie as colunas
df_filtrado$`microgap Pró Estado/Pró Mercado` <- df_filtrado$microGap_proMercado
df_filtrado$`microgap Liberal/Fundamentalismo` <- df_filtrado$microGap_fundamentalismo

# Reorganize os dados em formato longo
df_long <- df_filtrado %>%
  pivot_longer(cols = c(`microgap Liberal/Fundamentalismo`, `microgap Pró Estado/Pró Mercado`),
               names_to = "Medida",
               values_to = "Valor")

# Crie um único gráfico com as duas medidas contínuas
combined_plot <- ggplot(df_long, aes(x = partido, y = Valor, fill = partido)) +
  geom_boxplot() +
  facet_grid(Medida ~ ano, scales = "free_y") +
  labs(
    subtitle = "Micro Gap Liberal / Fundamentalismo e Micro Gap Pró Mercado / Pró Estado",
    x = "",
    y = "",
    color = "Partido"
  ) +
  scale_fill_manual(values = colors) +  # Use uma paleta de cores definida
  theme_bw() +
  coord_flip() +
  stat_summary(fun = "mean", geom = "point", size = 3, shape = 21, fill = "white", colour = "black") 

# Imprima o gráfico combinado
print(combined_plot)


library(tidyquant)

# Crie o histograma com densidade ou percentagem para a dimensão econômica
df_proMercado <- df %>% 
  dplyr::filter(ano != "Out 2018-2019") %>%
  ggplot(aes(x = proMercado, y = ..density..)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(
    subtitle = "Dimensão econômica (Pró Estado / Pró Mercado)",
    y = "",x="",
    fill = ""
  ) +
  facet_grid(ano ~ base) +
  theme_bw()

# Imprima o histograma
df_proMercado + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
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

# Imprima o histograma
df_fundamentalismo+theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())



