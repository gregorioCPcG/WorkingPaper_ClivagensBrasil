# grafico das distribuições comparadas
#OBS rodar no projeto distribuni
# ideia remover _out2018_2019!!!!!!!!!!!
#recod
library(broom)
library(readr)
df_t1 <- read_csv("df_t1.csv")
df_t2 <- read_csv("df_t2.csv")
df_t3 <- read_csv("df_t3.csv")
df_t4 <- read_csv("df_t4.csv")
df_t5 <- read_csv("df_t5.csv")
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
ordem_desejada <- c("2005-2006", "2014-2014", "2017-2018", "2021-2019")

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
#pro mercado

# Função para realizar o teste t e extrair os resultados
perform_t_test <- function(data) {
  test_result <- t.test(data$proMercado[data$base == "Elite"], data$proMercado[data$base == "Público"])
  p_value <- test_result$p.value
  ci_lower <- test_result$conf.int[1]
  ci_upper <- test_result$conf.int[2]
  mean_sdl <- mean(test_result$conf.int)
  
  result <- data.frame(
    partido = unique(data$partido),
    p_value = p_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    mean_sdl = mean_sdl
  )
  
  return(result)
}

# Aplicar o teste t para cada partido
test_results_proMercado_t1 <- df_t1 %>%
  group_by(partido) %>%
  do(perform_t_test(.))

# Verificar os resultados
print(test_results_proMercado_t1)
test_results_proMercado_t1 <- test_results_proMercado_t1 %>% mutate(ano = "2005-2006")
test_results_proMercado_t1 %>% glimpse()

# Aplicar o teste t para cada partido
test_results_proMercado_t2 <- df_t2 %>%
  group_by(partido) %>%
  do(perform_t_test(.))

# Verificar os resultados
print(test_results_proMercado_t2)
test_results_proMercado_t2 <- test_results_proMercado_t2 %>% mutate(ano = "2014-2014")
test_results_proMercado_t2 %>% glimpse()

# Aplicar o teste t para cada partido
test_results_proMercado_t3 <- df_t3 %>%
  group_by(partido) %>%
  do(perform_t_test(.))

# Verificar os resultados
print(test_results_proMercado_t3)
test_results_proMercado_t3 <- test_results_proMercado_t3 %>% mutate(ano = "2017-2018")
test_results_proMercado_t3 %>% glimpse()

# Aplicar o teste t para cada partido
test_results_proMercado_t4 <- df_t4 %>%
  group_by(partido) %>%
  do(perform_t_test(.))

# Verificar os resultados
print(test_results_proMercado_t4)
test_results_proMercado_t4 <- test_results_proMercado_t4 %>% mutate(ano = "Out 2018-2019")
test_results_proMercado_t4 %>% glimpse()


# Aplicar o teste t para cada partido
test_results_proMercado_t5 <- df_t5 %>%
  group_by(partido) %>%
  do(perform_t_test(.))

# Verificar os resultados
print(test_results_proMercado_t5)
test_results_proMercado_t5 <- test_results_proMercado_t5 %>% mutate(ano = "2021-2019")
test_results_proMercado_t5 %>% glimpse()



proMercadofinal <- rbind(test_results_proMercado_t1,
                         test_results_proMercado_t2,
                         test_results_proMercado_t3,
                         test_results_proMercado_t4,
                         test_results_proMercado_t5)
proMercadofinal  %>% glimpse()
table(proMercadofinal$ano)#para conferir
# Defina a ordem desejada das categorias
ordem_desejada <- c("2005-2006", "2014-2014", "2017-2018", "Out 2018-2019", "2021-2019")

# Use a função factor para reordenar a variável ano
proMercadofinal$ano <- factor(proMercadofinal$ano, levels = ordem_desejada)

# Verifique se a ordem foi definida corretamente
levels(proMercadofinal$ano)




proMercadofinal %>%
  dplyr::filter(partido == "PT") %>%
  ggplot(aes(x = ano, y = mean_sdl)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "PT - dimensão econômica",
       x = "",
       y = "") +
  theme_minimal()

table(proMercadofinal$partido)
#unificar MDB com PMDB
proMercadofinal <- proMercadofinal %>%
  mutate(partido = ifelse(partido %in% c("MDB", "PMDB"), "MDB/PMDB", partido))

# Verifique se funcionou
table(proMercadofinal$partido)

proMercadofinal %>%
  dplyr::filter(partido == "MDB/PMDB") %>%
  ggplot(aes(x = ano, y = mean_sdl)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "MDB/PMDB - dimensão econômica",
       x = "",
       y = "") +
  theme_minimal()


proMercadofinal %>%
  dplyr::filter(partido == "PDT") %>%
  ggplot(aes(x = ano, y = mean_sdl)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "PDT - dimensão econômica",
       x = "",
       y = "") +
  theme_minimal()

proMercadofinal %>%
  dplyr::filter(partido == "PFL") %>%
  ggplot(aes(x = ano, y = mean_sdl)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "PFL - dimensão econômica",
       x = "",
       y = "") +
  theme_minimal()



proMercadofinal %>%
  dplyr::filter(partido == "PSDB") %>%
  ggplot(aes(x = ano, y = mean_sdl)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "PSDB - dimensão econômica",
       x = "",
       y = "") +
  theme_minimal()


proMercadofinal %>%
  dplyr::filter(partido == "PSL") %>%
  ggplot(aes(x = ano, y = mean_sdl)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "PSL - dimensão econômica",
       x = "",
       y = "") +
  theme_minimal()

proMercadofinal %>%
  dplyr::filter(partido == "PSOL") %>%
  ggplot(aes(x = ano, y = mean_sdl)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "PSOL - dimensão econômica",
       x = "",
       y = "") +
  theme_minimal()

proMercadofinal %>%
  dplyr::filter(partido == "PSB") %>%
  ggplot(aes(x = ano, y = mean_sdl)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "PSB - dimensão econômica",
       x = "",
       y = "") +
  theme_minimal()

proMercadofinal %>%
  dplyr::filter(partido == "PTB") %>%
  ggplot(aes(x = ano, y = mean_sdl)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "PTB - dimensão econômica",
       x = "",
       y = "") +
  theme_minimal()

#  df$fundamentalismo
# Função para realizar o teste t e extrair os resultados
perform_t_test <- function(data) {
  test_result <- t.test(data$fundamentalismo[data$base == "Elite"], data$fundamentalismo[data$base == "Público"])
  p_value <- test_result$p.value
  ci_lower <- test_result$conf.int[1]
  ci_upper <- test_result$conf.int[2]
  mean_sdl <- mean(test_result$conf.int)
  
  result <- data.frame(
    partido = unique(data$partido),
    p_value = p_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    mean_sdl = mean_sdl
  )
  
  return(result)
}

# Aplicar o teste t para cada partido
test_results_fundamentalismo_t1 <- df_t1 %>%
  group_by(partido) %>%
  do(perform_t_test(.))

# Verificar os resultados
print(test_results_fundamentalismo_t1)
test_results_fundamentalismo_t1 <- test_results_fundamentalismo_t1 %>% mutate(ano = "2005-2006")
test_results_fundamentalismo_t1 %>% glimpse()

# Aplicar o teste t para cada partido
test_results_fundamentalismo_t2 <- df_t2 %>%
  group_by(partido) %>%
  do(perform_t_test(.))

# Verificar os resultados
print(test_results_fundamentalismo_t2)
test_results_fundamentalismo_t2 <- test_results_fundamentalismo_t2 %>% mutate(ano = "2014-2014")
test_results_fundamentalismo_t2 %>% glimpse()

# Aplicar o teste t para cada partido
test_results_fundamentalismo_t3 <- df_t3 %>%
  group_by(partido) %>%
  do(perform_t_test(.))

# Verificar os resultados
print(test_results_fundamentalismo_t3)
test_results_fundamentalismo_t3 <- test_results_fundamentalismo_t3 %>% mutate(ano = "2017-2018")
test_results_fundamentalismo_t3 %>% glimpse()

# Aplicar o teste t para cada partido
test_results_fundamentalismo_t4 <- df_t4 %>%
  group_by(partido) %>%
  do(perform_t_test(.))

# Verificar os resultados
print(test_results_fundamentalismo_t4)
test_results_fundamentalismo_t4 <- test_results_fundamentalismo_t4 %>% mutate(ano = "Out 2018-2019")
test_results_fundamentalismo_t4 %>% glimpse()


# Aplicar o teste t para cada partido
test_results_fundamentalismo_t5 <- df_t5 %>%
  group_by(partido) %>%
  do(perform_t_test(.))

# Verificar os resultados
print(test_results_fundamentalismo_t5)
test_results_fundamentalismo_t5 <- test_results_fundamentalismo_t5 %>% mutate(ano = "2021-2019")
test_results_fundamentalismo_t5 %>% glimpse()



fundamentalismofinal <- rbind(test_results_fundamentalismo_t1,
                              test_results_fundamentalismo_t2,
                              test_results_fundamentalismo_t3,
                              test_results_fundamentalismo_t4,
                              test_results_fundamentalismo_t5)
fundamentalismofinal  %>% glimpse()
table(fundamentalismofinal $ano)#para conferir

# Use a função factor para reordenar a variável ano
fundamentalismofinal$ano <- factor(fundamentalismofinal$ano, levels = ordem_desejada)

# Verifique se a ordem foi definida corretamente
levels(fundamentalismofinal$ano)



fundamentalismofinal %>%
  dplyr::filter(partido == "PT") %>%
  ggplot(aes(x = ano, y = mean_sdl)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "PT - dimensão cultural",
       x = "",
       y = "") +
  theme_minimal()

table(fundamentalismofinal$partido)
#unificar MDB com PMDB
fundamentalismofinal <- fundamentalismofinal %>%
  mutate(partido = ifelse(partido %in% c("MDB", "PMDB"), "MDB/PMDB", partido))

# Verifique se funcionou
table(fundamentalismofinal$partido)

fundamentalismofinal %>%
  dplyr::filter(partido == "MDB/PMDB") %>%
  ggplot(aes(x = ano, y = mean_sdl)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "MDB/PMDB - dimensão cultural",
       x = "",
       y = "") +
  theme_minimal()


fundamentalismofinal %>%
  dplyr::filter(partido == "PDT") %>%
  ggplot(aes(x = ano, y = mean_sdl)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "PDT - dimensão cultural",
       x = "",
       y = "") +
  theme_minimal()

fundamentalismofinal %>%
  dplyr::filter(partido == "PFL") %>%
  ggplot(aes(x = ano, y = mean_sdl)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "PFL - dimensão cultural",
       x = "",
       y = "") +
  theme_minimal()



fundamentalismofinal %>%
  dplyr::filter(partido == "PSDB") %>%
  ggplot(aes(x = ano, y = mean_sdl)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "PSDB - dimensão cultural",
       x = "",
       y = "") +
  theme_minimal()


fundamentalismofinal %>%
  dplyr::filter(partido == "PSL") %>%
  ggplot(aes(x = ano, y = mean_sdl)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "PSL - dimensão cultural",
       x = "",
       y = "") +
  theme_minimal()

fundamentalismofinal %>%
  dplyr::filter(partido == "PSOL") %>%
  ggplot(aes(x = ano, y = mean_sdl)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "PSOL - dimensão cultural",
       x = "",
       y = "") +
  theme_minimal()

fundamentalismofinal %>%
  dplyr::filter(partido == "PSB") %>%
  ggplot(aes(x = ano, y = mean_sdl)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "PSB - dimensão cultural",
       x = "",
       y = "") +
  theme_minimal()

fundamentalismofinal %>%
  dplyr::filter(partido == "PTB") %>%
  ggplot(aes(x = ano, y = mean_sdl)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "PTB - dimensão cultural",
       x = "",
       y = "") +
  theme_minimal()


#
#otimo

# agora as do apendice

# Renomear "MDB/PMDB" para "MDB"
fundamentalismofinal$partido <- ifelse(fundamentalismofinal$partido == "MDB/PMDB", "MDB", fundamentalismofinal$partido)

# Verifique se a mudança foi feita corretamente
table(fundamentalismofinal$partido)

# Defina a ordem desejada dos partidos
ordem_desejada <- c("PSOL", "PT", "PDT", "PSB", "MDB", "PSDB", "PFL", "PSL", "PTB")

# Reordene a variável partido
fundamentalismofinal$partido <- factor(fundamentalismofinal$partido, levels = ordem_desejada)

# Verifique se a ordem foi definida corretamente
table(fundamentalismofinal$partido)

# Renomear "MDB/PMDB" para "MDB"
proMercadofinal$partido <- ifelse(proMercadofinal$partido == "MDB/PMDB", "MDB", proMercadofinal$partido)

# Verifique se a mudança foi feita corretamente
table(proMercadofinal$partido)

# Defina a ordem desejada dos partidos
ordem_desejada <- c("PSOL", "PT", "PDT", "PSB", "MDB", "PSDB", "PFL", "PSL", "PTB")

# Reordene a variável partido
proMercadofinal$partido <- factor(proMercadofinal$partido, levels = ordem_desejada)

# Verifique se a ordem foi definida corretamente
table(proMercadofinal$partido)

#tabelas

fundamentalismofinal %>%
  #dplyr::filter(partido == "PTB") %>%
  ggplot(aes(x = partido, y = mean_sdl)) +
  geom_point(size = 3) + facet_wrap(~ano)+
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "T-Test por partido Liberal/Fundamentalismo",
       x = "",
       y = "") +
  theme_minimal()+ coord_flip()

proMercadofinal %>%
  #dplyr::filter(partido == "PTB") %>%
  ggplot(aes(x = partido, y = mean_sdl)) +
  geom_point(size = 3) + facet_wrap(~ano)+
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "T-Test por partido dimensão econômica",
       x = "",
       y = "") +
  theme_minimal()+ coord_flip()

# separei por esq,centro e direita


#ideologia

fundamentalismofinal <- fundamentalismofinal %>%
  mutate(ideologia = case_when(
    partido %in% c("PSOL", "PT", "PDT", "PSB") ~ "Esquerda",
    partido %in% c("MDB", "PSDB") ~ "Centro",
    TRUE ~ "Direita"
  ))
proMercadofinal <- proMercadofinal %>%
  mutate(ideologia = case_when(
    partido %in% c("PSOL", "PT", "PDT", "PSB") ~ "Esquerda",
    partido %in% c("MDB", "PSDB") ~ "Centro",
    TRUE ~ "Direita"
  ))

fundamentalismofinal%>% glimpse()
proMercadofinal %>% glimpse()



fundamentalismofinal %>%
  ggplot(aes(x= ano, y = mean_sdl)) +
  geom_point(size = 3) +
  facet_wrap(~ideologia) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "T-Test por partido Liberal/Fundamentalismo",
       x = "",
       y = "") +
  theme_minimal()


proMercadofinal %>%
  ggplot(aes(x = ideologia, y = mean_sdl)) +
  geom_point(size = 3) + facet_wrap(~ano)+
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(subtitle = "T-Test por partido dimensão econômica",
       x = "",
       y = "") +
  theme_minimal()



fundamentalismofinal %>%
  filter(ideologia %in% c("Esquerda", "Centro", "Direita")) %>%
  group_by(ano, ideologia) %>%
  summarize(mean_sdl = mean(mean_sdl),
            ci_lower = mean(ci_lower),
            ci_upper = mean(ci_upper)) %>%
  mutate(ideologia = factor(ideologia, levels = c("Esquerda", "Centro", "Direita"))) %>%
  ggplot(aes(x = ideologia, y = mean_sdl, group = ano)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~ano) +
  labs(subtitle = "T-Test por Ideologia Liberal/Fundamentalismo",
       x = "",
       y = "") +
  theme_minimal()+ coord_flip()


proMercadofinal %>%
  filter(ideologia %in% c("Esquerda", "Centro", "Direita")) %>%
  group_by(ano, ideologia) %>%
  summarize(mean_sdl = mean(mean_sdl),
            ci_lower = mean(ci_lower),
            ci_upper = mean(ci_upper)) %>%
  mutate(ideologia = factor(ideologia, levels = c("Esquerda", "Centro", "Direita"))) %>%
  ggplot(aes(x = ideologia, y = mean_sdl, group = ano)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~ano) +
  labs(subtitle = "T-Test por Ideologia dimensão econômica",
       x = "",
       y = "") +
  theme_minimal()+ coord_flip()




