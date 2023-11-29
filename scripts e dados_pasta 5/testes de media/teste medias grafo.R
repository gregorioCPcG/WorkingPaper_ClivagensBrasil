library(ggplot2)

# Definir os dados
periodos <- c("2005-2006", "2014-2014", "2018-2018", "2018 out-2019", "2021-2019")
diferencas_medias <- c(-0.2487305, -0.1937144, -0.2381865, -0.3555346, -0.3777312)
intervalos_confianca_inf <- c(-0.3004076, -0.2595775, -0.3126582, -0.4653881, -0.4810322)
intervalos_confianca_sup <- c(-0.1950533, -0.1278513, -0.1569708, -0.2462930, -0.2744302)

# Criar um data frame com os dados
dados <- data.frame(Periodo = factor(periodos, levels = periodos),
                    Diferenca_Medias = diferencas_medias,
                    Intervalo_Confianca_Inferior = intervalos_confianca_inf,
                    Intervalo_Confianca_Superior = intervalos_confianca_sup)

# Criar o gráfico de barras
grafico_barras <- ggplot(dados, aes(x = Periodo, y = Diferenca_Medias)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  geom_errorbar(aes(ymin = Intervalo_Confianca_Inferior, ymax = Intervalo_Confianca_Superior),
                width = 0.2, position = position_dodge(width = 0.9), color = "black") +
  labs(y = "Média da Elite - Média Povo", x = "Período", subtitle="Liberal Fundamentalismo adaptado") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exibir o gráfico
print(grafico_barras)

#Deu certo
#agora
#gráfico completo
#

#
library(ggplot2)
options(scipen = 999)
# Dados para Liberal Fundamentalismo adaptado
periodos_liberal <- c("2005-2006", "2014-2014", "2018-2018", "2018 out-2019", "2021-2019")
diferencas_medias_liberal <- c(-0.2487305, -0.1937144, -0.2381865, -0.3555346, -0.3777312)
intervalos_confianca_inf_liberal <- c(-0.3004076, -0.2595775, -0.3126582, -0.4653881, -0.4810322)
intervalos_confianca_sup_liberal <- c(-0.1950533, -0.1278513, -0.1569708, -0.2462930, -0.2744302)
p_values_liberal <- c(0.00000000000001082, 0.00000008485, 0.00000007651, 0.00000005782, 0.00000000318)
significancia_liberal <- ifelse(p_values_liberal < 0.05, "Significativo", "Não Significativo")

# Criar um data frame para Liberal Fundamentalismo adaptado
dados_liberal <- data.frame(
  Periodo = factor(periodos_liberal, levels = periodos_liberal),
  Diferenca_Medias = diferencas_medias_liberal,
  Intervalo_Confianca_Inferior = intervalos_confianca_inf_liberal,
  Intervalo_Confianca_Superior = intervalos_confianca_sup_liberal,
  P_Value = p_values_liberal,
  Significância = significancia_liberal,
  Dimensao = "Liberal Fundamentalismo adaptado"
)

# Dados para Pró Estado / Pró Mercado
periodos_pro_estado <- c("2005-2006", "2014-2014", "2018-2018", "2018 out-2019", "2021-2019")
diferencas_medias_pro_estado <- c(0.0757808, -0.0472193, 0.1612923, 0.1374638, 0.1588924)
p_values_pro_estado <- c(0.00385, 0.2179, 0.001673, 0.04342, 0.02091)
significancia_pro_estado <- ifelse(p_values_pro_estado < 0.05, "Significativo", "Não Significativo")

intervalos_confianca_inf_pro_estado <- c(0.03192487, -0.1227022, 0.06269273, 0.004251945, 0.02526291)
intervalos_confianca_sup_pro_estado <- c(0.16163668, 0.0282634, 0.25967569, 0.270675642, 0.29252182)

# Criar um data frame para Pró Estado / Pró Mercado
dados_pro_estado <- data.frame(
  Periodo = factor(periodos_pro_estado, levels = periodos_pro_estado),
  Diferenca_Medias = diferencas_medias_pro_estado,
  P_Value = p_values_pro_estado,
  Significância = significancia_pro_estado,
  Intervalo_Confianca_Inferior = intervalos_confianca_inf_pro_estado,
  Intervalo_Confianca_Superior = intervalos_confianca_sup_pro_estado,
  Dimensao = "Pró Estado / Pró Mercado"
)

# Combinar os dois data frames
dados_combinados <- rbind(dados_liberal, dados_pro_estado)

# Criar o gráfico com facet_wrap
grafico_combinado <- ggplot(dados_combinados, aes(x = Periodo, y = Diferenca_Medias, fill = Significância)) +
  geom_errorbar(aes(ymin = Intervalo_Confianca_Inferior, ymax = Intervalo_Confianca_Superior),
                width = 0.4, position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange",
               position = position_dodge(width = 0.9), color = "black", fill = "white") +
  labs(y = "Diferença de Médias (Elite - Povo)", x = "Período",
       subtitle = "") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Adiciona a linha vermelha
  facet_wrap(~ Dimensao, scales = "free_y") +
  scale_fill_manual(values = c("gray" = "gray", "Não Significativo" = "gray",
                               "Significativo" = "blue")) +
  theme_minimal()

grafico_combinado
library(ggdist)
library(tidyquant)
# tentando ver como fica virado
grafico_combinado + coord_flip()
grafico_combinado + coord_flip()+ theme_tq()
grafico_salvar <- grafico_combinado + coord_flip()+ theme_tq()
# acima temos com a nossa base dos minimos 20 partidarios e 3 deputados

# agora com a base completa
library(ggplot2)
options(scipen = 999)

# Dados para Liberal Fundamentalismo adaptado
periodos_liberal <- c("2005-2006", "2014-2014", "2017-2018", "Out 2018-2019", "2021-2019")
diferencas_medias_liberal <- c(
  0.4828713 - 0.6545586,
  0.4736155 - 0.6026740,
  0.4051059 - 0.5822967,
  0.3254265 - 0.5919041,
  0.3254265 - 0.5919041
)
intervalos_confianca_inf_liberal <- c(-0.2155155, -0.18212540, -0.2327726, -0.3235571, -0.3235571)
intervalos_confianca_sup_liberal <- c(-0.1278591, -0.07599148, -0.1216091, -0.2093980, -0.2093980)
p_values_liberal <- c(0.000000000002346, 0.000004099, 0.000000003145, 0.0000000000000005925, 0.0000000000000005925)
significancia_liberal <- ifelse(p_values_liberal < 0.05, "Significativo", "Não Significativo")

# Criar um data frame para Liberal Fundamentalismo adaptado
dados_liberal <- data.frame(
  Periodo = factor(periodos_liberal, levels = periodos_liberal),
  Diferenca_Medias = diferencas_medias_liberal,
  Intervalo_Confianca_Inferior = intervalos_confianca_inf_liberal,
  Intervalo_Confianca_Superior = intervalos_confianca_sup_liberal,
  P_Value = p_values_liberal,
  Significância = significancia_liberal,
  Dimensao = "Liberal Fundamentalismo"
)

# Dados para Pró Estado / Pró Mercado
periodos_pro_estado <- c("2005-2006", "2014-2014", "2017-2018", "Out 2018-2019", "2021-2019")
diferencas_medias_pro_estado <- c(
  0.6030702 - 0.5042827,
  0.5362319 - 0.5204765,
  0.7313433 - 0.5614836,
  0.6607143 - 0.4459855,
  0.6607143 - 0.4459855
)
p_values_pro_estado <- c(0.00385, 0.5865, 0.00000002274, 0.000000001759, 0.000000001759)
significancia_pro_estado <- ifelse(p_values_pro_estado < 0.05, "Significativo", "Não Significativo")

intervalos_confianca_inf_pro_estado <- c(0.04650524, -0.04135497, 0.1126973, 0.1491288, 0.1491288)
intervalos_confianca_sup_pro_estado <- c(0.15106980, 0.07286565, 0.2270221, 0.2803287, 0.2803287)

# Criar um data frame para Pró Estado / Pró Mercado
dados_pro_estado <- data.frame(
  Periodo = factor(periodos_pro_estado, levels = periodos_pro_estado),
  Diferenca_Medias = diferencas_medias_pro_estado,
  P_Value = p_values_pro_estado,
  Significância = significancia_pro_estado,
  Intervalo_Confianca_Inferior = intervalos_confianca_inf_pro_estado,
  Intervalo_Confianca_Superior = intervalos_confianca_sup_pro_estado,
  Dimensao = "Pró Estado / Pró Mercado"
)

# Combinar os dois data frames
dados_combinados <- rbind(dados_liberal, dados_pro_estado)

# Criar o gráfico com facet_wrap
grafico_combinado <- ggplot(dados_combinados, aes(x = Periodo, y = Diferenca_Medias, fill = Significância)) +
  geom_errorbar(aes(ymin = Intervalo_Confianca_Inferior, ymax = Intervalo_Confianca_Superior),
                width = 0.4, position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange",
               position = position_dodge(width = 0.9), color = "black", fill = "white") +
  labs(y = "Diferença de Médias (Elite - Povo)", x = "Período",
       subtitle = "") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Adiciona a linha vermelha
  facet_wrap(~ Dimensao, scales = "free_y") +
  scale_fill_manual(values = c("gray" = "gray", "Não Significativo" = "gray",
                               "Significativo" = "blue")) +
  theme_minimal()
grafico_combinado

# Exibir o gráfico combinado
print(grafico_combinado)
grafico_combinado + coord_flip()
grafico_combinado<- grafico_combinado + coord_flip()+ theme_tq()
graf1 <- grafico_combinado + labs(subtitle = "Todos que responderam às questões",
                                  title = "A")+
  ylim(-0.5, 0.34)+theme(
    plot.title = element_text(hjust = 0.5),     # Centralizar o título horizontalmente
    plot.subtitle = element_text(hjust = 0.5)  # Centralizar o subtítulo horizontalmente
  )
graf2 <- grafico_salvar + labs(subtitle = "Somente aqueles pertencentes à partidos com 20 apoiadores e 3 deputados (os selecionados)",
                               title = "B")+
  ylim(-0.5, 0.34)+theme(
    plot.title = element_text(hjust = 0.5),     # Centralizar o título horizontalmente
    plot.subtitle = element_text(hjust = 0.5)  # Centralizar o subtítulo horizontalmente
  )

graf1
graf2

library(gridExtra)
grid.arrange(graf1,graf2)
