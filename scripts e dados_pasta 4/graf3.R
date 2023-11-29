#rodar grafico microgap.R
library(dplyr)
table(merge$partido)
merge$partido != "PT" -> merge$PTNao
merge$PTNao <- as.factor(merge$PTNao)
levels(merge$PTNao)
levels(merge$PTNao) <- c('Todos os partidos elegíveis','Sem considerar o PT')
levels(merge$PTNao)
table(merge$PTNao)


library(Rmisc)

ordem_desejada <- c("2021-2019","Out 2018-2019","2017-2018",
                    "2014-2014", "2005-2006")
merge$Período <- factor(merge$Período, levels = ordem_desejada)

graf3 <- merge %>%
  summarySE('microGap', c('Período', 'Dimensão','PTNao')) %>%
  ggplot(aes(x = microGap, y = PTNao, color = Período)) + facet_wrap(.~Dimensão)+
  geom_point(position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(xmin = microGap - ci, xmax = microGap + ci), 
                width = 0.5, 
                position = position_dodge(width = 0.7)) +
  #theme_linedraw() +
  labs(x = 'Micro Gap', y = '', 
       color = 'Dimensão') +
  theme(panel.grid = element_blank(),
        legend.position = 'bottom')


graf3 + theme_bw()
graf3 + theme_light()
graf3 + theme_tq()

