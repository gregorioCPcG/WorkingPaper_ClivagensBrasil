
####Fonte dos dados####

#WVS1997 - https://www.worldvaluessurvey.org/WVSDocumentationWV3.jsp
#WVS2006 - https://www.worldvaluessurvey.org/WVSDocumentationWV5.jsp
#WVS2014 - https://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp
#WVS2018 - https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp
#A Cara da Democracia (2019) - https://www.institutodademocracia.org/a-cara-da-democracia
#Banco das Clivagens (2023) - Dados primarios

####Pacotes utilizados####

##Install packagees

library(readxl)
library(dplyr)
library(lavaan)
library(ggplot2)
library(semPlot)
library(psych)
library(GPArotation)
library(mirt)
library(ggrepel)
library(tidyverse)
library(officer)
library(flextable)
library(haven)
library(readr)
library(sjPlot)
library(marginaleffects)
library(ggdist)
library(tidyquant)
library(gridExtra)
library(olsrr)
library(jtools)
library(moments)
library(lmtest)
library(DescTools)
library(lmtest)
library(sandwich)
library(broom)

####Recodificação dos Bancos####

#WVS1997

Dataframe <- read_excel("~/Banco de Dados/WVS/Dados/F00008090-WV3_Data_Brazil_Excel_v20221107.xlsx")

#Important in life: Religion

table(Dataframe$`V9: Important in life: Religion`)

Dataframe$V9 <- recode(Dataframe$`V9: Important in life: Religion`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V9 <- ifelse(Dataframe$V9 <= -1, NA, Dataframe$V9)

table(Dataframe$V9)

#Neigh Homo

table(Dataframe$`V60: Neighbours: Homosexuals`)

Dataframe$V60 <- recode(Dataframe$`V60: Neighbours: Homosexuals`, `2` = 0)

table(Dataframe$V60)

#Believe: God

table(Dataframe$`V183: Believe in: God`)

Dataframe$V183 <- recode(Dataframe$`V183: Believe in: God`, `2` = 0)

Dataframe$V183 <- ifelse(Dataframe$V183 <= -1, NA, Dataframe$V183)

table(Dataframe$V183)

#Importance of God

table(Dataframe$`V190: Importance of God in your life`)

Dataframe$V190 <- ifelse(Dataframe$`V190: Importance of God in your life` <= -1, NA, Dataframe$`V190: Importance of God in your life`)

table(Dataframe$V190)

#Sexual freedom

table(Dataframe$`V95: Enjoy sexual freedom`)

Dataframe$V95 <- recode(Dataframe$`V95: Enjoy sexual freedom`, `1` = 3, `2` = 1, `3` = 2)

Dataframe$V95 <- ifelse(Dataframe$V95 <= -1, NA, Dataframe$V95)

table(Dataframe$V95)

#Child faith

table(Dataframe$`V22: Important child qualities: religious faith`)

Dataframe$V22 <- recode(Dataframe$`V22: Important child qualities: religious faith`, `2` = 0)

Dataframe$V22 <- ifelse(Dataframe$V22 <= -1, NA, Dataframe$V22)

table(Dataframe$V22)

#Homosexuality

table(Dataframe$`V197: Justifiable: Homosexuality`)

Dataframe$V197 <- ifelse(Dataframe$`V197: Justifiable: Homosexuality` <= -1, NA, Dataframe$`V197: Justifiable: Homosexuality`)

table(Dataframe$V197)

#Abortion

table(Dataframe$`V199: Justifiable: Abortion`)

Dataframe$V199 <- ifelse(Dataframe$`V199: Justifiable: Abortion` <= -1, NA, Dataframe$`V199: Justifiable: Abortion`)

table(Dataframe$V199)

#Divorce

table(Dataframe$`V200: Justifiable: Divorce`)

Dataframe$V200 <- ifelse(Dataframe$`V200: Justifiable: Divorce` <= -1, NA, Dataframe$`V200: Justifiable: Divorce`)

table(Dataframe$V200)

#child obedience

table(Dataframe$`V24: Important child qualities: obedience`)

Dataframe$V24 <- recode(Dataframe$`V24: Important child qualities: obedience`, `2` = 0)

Dataframe$V24 <- ifelse(Dataframe$V24 <= -1, NA, Dataframe$V24)

table(Dataframe$V24)

#Woman single parent

table(Dataframe$`V96: Woman as a single parent`)

Dataframe$V96 <- recode(Dataframe$`V96: Woman as a single parent`, `1` = 3, `2` = 1, `3` = 2)

Dataframe$V96 <- ifelse(Dataframe$V96 <= -1, NA, Dataframe$V96)

table(Dataframe$V96)

#Societal change

table(Dataframe$`V124: Basic kinds of attitudes concerning society`)

Dataframe$V124 <- ifelse(Dataframe$`V124: Basic kinds of attitudes concerning society` <= -1, NA, Dataframe$`V124: Basic kinds of attitudes concerning society`)

table(Dataframe$V124)

#Aims of country

table(Dataframe$`V104: Aims of country: first choice`)

Dataframe$V104 <- ifelse(Dataframe$`V104: Aims of country: first choice` == 2, 2, 0)

table(Dataframe$V104)

table(Dataframe$`V105: Aims of country: second choice`)

Dataframe$V105 <- ifelse(Dataframe$`V105: Aims of country: second choice` == 2, 1, 0)

table(Dataframe$V105)

Dataframe$country <- Dataframe$V104 + Dataframe$V105

table(Dataframe$country)

#Aims of respondent

table(Dataframe$`V106: Aims of respondent: first choice`)

Dataframe$V106 <- ifelse(Dataframe$`V106: Aims of respondent: first choice` == 1, 2, 0)

table(Dataframe$V106)

table(Dataframe$`V107: Aims of respondent: second choice`)

Dataframe$V107 <- ifelse(Dataframe$`V107: Aims of respondent: second choice` == 1, 1, 0)

table(Dataframe$V107)

Dataframe$respondent <- Dataframe$V106 + Dataframe$V107

table(Dataframe$respondent)

#Most important

table(Dataframe$`V108: Most important: first choice`)

Dataframe$V108 <- ifelse(Dataframe$`V108: Most important: first choice` == 4, 2, 0)

table(Dataframe$V108)

table(Dataframe$`V109: Most important: second choice`)

Dataframe$V109 <- ifelse(Dataframe$`V109: Most important: second choice` == 4, 1, 0)

table(Dataframe$V109)

Dataframe$most <- Dataframe$V108 + Dataframe$V109

table(Dataframe$most)

#Respect for authority

table(Dataframe$`V114: Future changes: Greater respect for authority`)

Dataframe$V114 <- recode(Dataframe$`V114: Future changes: Greater respect for authority`, `1` = 3, `3` = 1)

Dataframe$V114 <- ifelse(Dataframe$V114 <= -1, NA, Dataframe$V114)

table(Dataframe$V114)

#National pride

table(Dataframe$`V205: How proud of nationality`)

Dataframe$V205 <- recode(Dataframe$`V205: How proud of nationality`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V205 <- ifelse(Dataframe$V205 <= -1, NA, Dataframe$V205)

Dataframe$V205 <- ifelse(Dataframe$V205 >= 5, NA, Dataframe$V205)

table(Dataframe$V205)

#Change

table(Dataframe$`V131: Major changes in life`)

Dataframe$V131 <- ifelse(Dataframe$`V131: Major changes in life` <= -1, NA, Dataframe$`V131: Major changes in life`)

table(Dataframe$V131)

#New old ideias

table(Dataframe$`V132: New and old ideas`)

Dataframe$V132 <- ifelse(Dataframe$`V132: New and old ideas` <= -1, NA, Dataframe$`V132: New and old ideas`)

table(Dataframe$V132)

#Tradition or growth

table(Dataframe$`V47: Tradition vs. high economic growth`)

Dataframe$V47 <- ifelse(Dataframe$`V47: Tradition vs. high economic growth` <= -1, NA, Dataframe$`V47: Tradition vs. high economic growth`)

Dataframe$V47 <- ifelse(Dataframe$V47 == 3, NA, Dataframe$V47)

table(Dataframe$V47)

#Political leader

table(Dataframe$`V101: Men make better political leaders than women do`)

Dataframe$V101 <- recode(Dataframe$`V101: Men make better political leaders than women do`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V101 <- ifelse(Dataframe$V101 <= -1, NA, Dataframe$V101)

table(Dataframe$V101)

#Order vs Freedom

table(Dataframe$`V159: Government order vs. freedom`)

Dataframe$V159 <- ifelse(Dataframe$`V159: Government order vs. freedom` <= -1, NA, Dataframe$`V159: Government order vs. freedom`)

table(Dataframe$V159)

#Rating pol sist before

table(Dataframe$`V151: Rate political system as it was before`)

Dataframe$V151 <- ifelse(Dataframe$`V151: Rate political system as it was before` <= -1, NA, Dataframe$`V151: Rate political system as it was before`)

table(Dataframe$V151)

#Army rule

table(Dataframe$`V156: Political system: Having the army rule`)

Dataframe$V156 <- recode(Dataframe$`V156: Political system: Having the army rule`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V156 <- ifelse(Dataframe$V156 <= -1, NA, Dataframe$V156)

table(Dataframe$V156)

#Democracy economy

table(Dataframe$`V160: In democracy, the economic system runs badly`)

Dataframe$V160 <- recode(Dataframe$`V160: In democracy, the economic system runs badly`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V160 <- ifelse(Dataframe$V160 <= -1, NA, Dataframe$V160)

table(Dataframe$V160)

#Democracy Stability

table(Dataframe$`V161: Democracies are indecisive and have too much squabbling`)

Dataframe$V161 <- recode(Dataframe$`V161: Democracies are indecisive and have too much squabbling`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V161 <- ifelse(Dataframe$V161 <= -1, NA, Dataframe$V161)

table(Dataframe$V161)

#Democracy Order

table(Dataframe$`V162: Democracies aren´t good at maintaining order`)

Dataframe$V162 <- recode(Dataframe$`V162: Democracies aren´t good at maintaining order`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V162 <- ifelse(Dataframe$V162 <= -1, NA, Dataframe$V162)

table(Dataframe$V162)

#Democracy Better

table(Dataframe$`V163: Democracy may have problems but is better`)

Dataframe$V163 <- recode(Dataframe$`V163: Democracy may have problems but is better`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V163 <- ifelse(Dataframe$V163 <= -1, NA, Dataframe$V163)

table(Dataframe$V163)

#Jobs for man

table(Dataframe$`V61: Jobs scarce: Men should have more right to a job than women`)

Dataframe$V61 <- recode(Dataframe$`V61: Jobs scarce: Men should have more right to a job than women`, `1` = 3, `3` = 1)

Dataframe$V61 <- ifelse(Dataframe$V61 <= -1, NA, Dataframe$V61)

table(Dataframe$V61)

Dataframe$V1 <- Dataframe$V61

#Income equality

table(Dataframe$`V125: Income equality`)

Dataframe$V125 <- ifelse(Dataframe$`V125: Income equality` <= -1, NA, Dataframe$`V125: Income equality`)

table(Dataframe$V125)

Dataframe$V4 <- Dataframe$V125

#Private/public ownership

table(Dataframe$`V126: Private vs state ownership of business`)

Dataframe$V126 <- ifelse(Dataframe$`V126: Private vs state ownership of business` <= -1, NA, Dataframe$`V126: Private vs state ownership of business`)

table(Dataframe$V126)

Dataframe$V5 <- Dataframe$V126

#Government responsibility

table(Dataframe$`V127: Government responsibility`)

Dataframe$V127 <- ifelse(Dataframe$`V127: Government responsibility` <= -1, NA, Dataframe$`V127: Government responsibility`)

table(Dataframe$V127)

Dataframe$V13 <- Dataframe$V127

#Competition

table(Dataframe$`V128: Competition good or harmful`)

Dataframe$V128 <- ifelse(Dataframe$`V128: Competition good or harmful` <= -1, NA, Dataframe$`V128: Competition good or harmful`)

table(Dataframe$V128)

Dataframe$V6 <- Dataframe$V128

#INDEPENDENTES

#Classe

table(Dataframe$`V221: Profession/job`)

Dataframe$V221 <- recode(Dataframe$`V221: Profession/job`, `1` = 0, `10` = 0, `11` = 0, `12` = 0)

Dataframe$V221 <- ifelse(Dataframe$V221 <= 0, NA, Dataframe$V221)

table(Dataframe$V221)

#Religi?o

table(Dataframe$`v179: Religious denomination - detailed list`)

Dataframe$V179 <- recode(Dataframe$`v179: Religious denomination - detailed list`, `4e+07` = 0)

Dataframe$V179 <- ifelse(Dataframe$V179 <= 0, NA, Dataframe$V179)

table(Dataframe$V179)

#Partido

table(Dataframe$`V210: Which party would you vote for: First choice`)

Dataframe$V210 <- recode(Dataframe$`V210: Which party would you vote for: First choice`, `76004` = 0, `76005` = 0)

Dataframe$V210 <- ifelse(Dataframe$V210 <= 0, NA, Dataframe$V210)

Dataframe$V210 <- ifelse(Dataframe$V210 >= 76008, NA, Dataframe$V210)

table(Dataframe$V210)

#Partido negativo

table(Dataframe$`V212: Party that would never vote`)

Dataframe$V212 <- recode(Dataframe$`V212: Party that would never vote`, `76003` = 0, `76004` = 0, `76005` = 0, `76007` = 0, `76008` = 0)

Dataframe$V212 <- ifelse(Dataframe$V212 <= 0, NA, Dataframe$V212)

Dataframe$V212 <- ifelse(Dataframe$V212 >= 76010, NA, Dataframe$V212)

table(Dataframe$V212)

#Comparecimento servi?os religiosos

table(Dataframe$`V181: How often do you attend religious services`)

Dataframe$V181 <- recode(Dataframe$`V181: How often do you attend religious services`, `1` = 3, `2` = 3, `3` = 2, `4` = 2, `5` = 2, `6` = 1, `7` = 1)

Dataframe$V181 <- ifelse(Dataframe$V181 <= -1, NA, Dataframe$V181)

table(Dataframe$V181)

#Ideologia

table(Dataframe$`V123: Self positioning in political scale`)

Dataframe$V123 <- ifelse(Dataframe$`V123: Self positioning in political scale` <= -1, NA, Dataframe$`V123: Self positioning in political scale`)

table(Dataframe$V123)

Dataframe1997 <- Dataframe

#WVS2006

Dataframe <- read_excel("~/Banco de Dados/WVS/Dados/F00007825-WV5_Data_Brazil_Excel_v20201117.xlsx")

#Important in life: Religion

table(Dataframe$`V9: Important in life: Religion`)

Dataframe$V9 <- recode(Dataframe$`V9: Important in life: Religion`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V9 <- ifelse(Dataframe$V9 <= -1, NA, Dataframe$V9)

table(Dataframe$V9)

Dataframe$S1 <- Dataframe$V9

#Neigh Homo

table(Dataframe$`V38: Neighbours: Homosexuals`)

Dataframe$V38 <- recode(Dataframe$`V38: Neighbours: Homosexuals`, `2` = 0)

table(Dataframe$V38)

Dataframe$S2 <- Dataframe$V38

#Importance of God

table(Dataframe$`V192: How important is god in your life`)

Dataframe$V192 <- ifelse(Dataframe$`V192: How important is god in your life` <= -1, NA, Dataframe$`V192: How important is god in your life`)

table(Dataframe$V192)

Dataframe$S3 <- Dataframe$V192

#Child faith

table(Dataframe$`V19: Important child qualities: religious faith`)

Dataframe$V19 <- recode(Dataframe$`V19: Important child qualities: religious faith`, `2` = 0)

Dataframe$V19 <- ifelse(Dataframe$V19 <= -1, NA, Dataframe$V19)

table(Dataframe$V19)

Dataframe$S4 <- Dataframe$V19

#Homosexuality

table(Dataframe$`V202: Justifiable: Homosexuality`)

Dataframe$V202 <- ifelse(Dataframe$`V202: Justifiable: Homosexuality` <= -1, NA, Dataframe$`V202: Justifiable: Homosexuality`)

table(Dataframe$V202)

Dataframe$S5 <- Dataframe$V202

#Abortion

table(Dataframe$`V204: Justifiable: Abortion`)

Dataframe$V204 <- ifelse(Dataframe$`V204: Justifiable: Abortion` <= -1, NA, Dataframe$`V204: Justifiable: Abortion`)

table(Dataframe$V204)

Dataframe$S6 <- Dataframe$V204

#Divorce

table(Dataframe$`V205: Justifiable: Divorce`)

Dataframe$V205 <- ifelse(Dataframe$`V205: Justifiable: Divorce` <= -1, NA, Dataframe$`V205: Justifiable: Divorce`)

table(Dataframe$V205)

Dataframe$S7 <- Dataframe$V205

#child obedience

table(Dataframe$`V21: Important child qualities: obedience`)

Dataframe$V21 <- recode(Dataframe$`V21: Important child qualities: obedience`, `2` = 0)

Dataframe$V21 <- ifelse(Dataframe$V21 <= -1, NA, Dataframe$V21)

table(Dataframe$V21)

Dataframe$S8 <- Dataframe$V21

#Woman single parent

table(Dataframe$`V59: Woman as a single parent`)

Dataframe$V59 <- recode(Dataframe$`V59: Woman as a single parent`, `1` = 3, `2` = 1, `3` = 2)

Dataframe$V59 <- ifelse(Dataframe$V59 <= -1, NA, Dataframe$V59)

table(Dataframe$V59)

#Aims of country

table(Dataframe$`V69: Aims of country: first choice`)

Dataframe$V69 <- ifelse(Dataframe$`V69: Aims of country: first choice` == 2, 2, 0)

table(Dataframe$V69)

table(Dataframe$`V70: Aims of country: second choice`)

Dataframe$V70 <- ifelse(Dataframe$`V70: Aims of country: second choice` == 2, 1, 0)

table(Dataframe$V70)

Dataframe$country <- Dataframe$V69 + Dataframe$V70

table(Dataframe$country)

Dataframe$S9 <- Dataframe$country

#Aims of respondent

table(Dataframe$`V71: Aims of respondent: first choice`)

Dataframe$V71 <- ifelse(Dataframe$`V71: Aims of respondent: first choice` == 1, 2, 0)

table(Dataframe$V71)

table(Dataframe$`V72: Aims of respondent: second choice`)

Dataframe$V72 <- ifelse(Dataframe$`V72: Aims of respondent: second choice` == 1, 1, 0)

table(Dataframe$V72)

Dataframe$respondent <- Dataframe$V71 + Dataframe$V72

table(Dataframe$respondent)

Dataframe$S10 <- Dataframe$respondent

#Most important

table(Dataframe$`V73: Most important: first choice`)

Dataframe$V73 <- ifelse(Dataframe$`V73: Most important: first choice` == 4, 2, 0)

table(Dataframe$V73)

table(Dataframe$`V74: Most important: second choice`)

Dataframe$V74 <- ifelse(Dataframe$`V74: Most important: second choice` == 4, 1, 0)

table(Dataframe$V74)

Dataframe$most <- Dataframe$V73 + Dataframe$V74

table(Dataframe$most)

Dataframe$S11 <- Dataframe$most

#Respect for authority

table(Dataframe$`V78: Future changes: Greater respect for authority`)

Dataframe$V78 <- recode(Dataframe$`V78: Future changes: Greater respect for authority`, `1` = 3, `3` = 1)

Dataframe$V78 <- ifelse(Dataframe$V78 <= -1, NA, Dataframe$V78)

table(Dataframe$V78)

Dataframe$S12 <- Dataframe$V78

#National pride

table(Dataframe$`V209: How proud of nationality`)

Dataframe$V209 <- recode(Dataframe$`V209: How proud of nationality`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V209 <- ifelse(Dataframe$V209 <= -1, NA, Dataframe$V209)

Dataframe$V209 <- ifelse(Dataframe$V209 >= 5, NA, Dataframe$V209)

table(Dataframe$V209)

Dataframe$S13 <- Dataframe$V209

#Political leader

table(Dataframe$`V61: Men make better political leaders than women do`)

Dataframe$V61 <- recode(Dataframe$`V61: Men make better political leaders than women do`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V61 <- ifelse(Dataframe$V61 <= -1, NA, Dataframe$V61)

table(Dataframe$V61)

Dataframe$S14 <- Dataframe$V61

#Schwart

table(Dataframe$`V89: Schwartz: It is important to this person tradition`)

Dataframe$V89 <- recode(Dataframe$`V89: Schwartz: It is important to this person tradition`, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1)

Dataframe$V89 <- ifelse(Dataframe$V89 <= -1, NA, Dataframe$V89)

table(Dataframe$V89)

#Army rule

table(Dataframe$`V150: Political system: Having the army rule`)

Dataframe$V150 <- recode(Dataframe$`V150: Political system: Having the army rule`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V150 <- ifelse(Dataframe$V150 <= -1, NA, Dataframe$V150)

table(Dataframe$V150)

Dataframe$S15 <- Dataframe$V150

#Democracy: Army

table(Dataframe$`V156: Democracy: The army takes over when government is incompetent.`)

Dataframe$V156 <- ifelse(Dataframe$`V156: Democracy: The army takes over when government is incompetent.` <= -1, NA, Dataframe$`V156: Democracy: The army takes over when government is incompetent.`)

table(Dataframe$V156)

Dataframe$S16 <- Dataframe$V156

#Jobs for man

table(Dataframe$`V44: Jobs scarce: Men should have more right to a job than women`)

Dataframe$V44 <- recode(Dataframe$`V44: Jobs scarce: Men should have more right to a job than women`, `1` = 3, `3` = 1)

Dataframe$V44 <- ifelse(Dataframe$V44 <= -1, NA, Dataframe$V44)

table(Dataframe$V44)

Dataframe$S17 <- Dataframe$V44

#Democracy: Elections

table(Dataframe$`V154: Democracy: People choose their leaders in free elections.`)

Dataframe$V154 <- ifelse(Dataframe$`V154: Democracy: People choose their leaders in free elections.` <= -1, NA, Dataframe$`V154: Democracy: People choose their leaders in free elections.`)

table(Dataframe$V154)

Dataframe$S18 <- Dataframe$V154

#Democracy: Civil rights

table(Dataframe$`V157: Democracy: Civil rights protect people’s liberty against oppression.`)

Dataframe$V157 <- ifelse(Dataframe$`V157: Democracy: Civil rights protect people’s liberty against oppression.` <= -1, NA, Dataframe$`V157: Democracy: Civil rights protect people’s liberty against oppression.`)

table(Dataframe$V157)

Dataframe$S19 <- Dataframe$V157

#Democracy: Men = Woman

table(Dataframe$`V161: Democracy: Women have the same rights as men.`)

Dataframe$V161 <- ifelse(Dataframe$`V161: Democracy: Women have the same rights as men.` <= -1, NA, Dataframe$`V161: Democracy: Women have the same rights as men.`)

table(Dataframe$V161)

Dataframe$S20 <- Dataframe$V161

#Income equality

table(Dataframe$`V116: Income equality`)

Dataframe$V116 <- ifelse(Dataframe$`V116: Income equality` <= -1, NA, Dataframe$`V116: Income equality`)

table(Dataframe$V116)

Dataframe$S21 <- Dataframe$V116

#Private/public ownership

table(Dataframe$`V117: Private vs state ownership of business`)

Dataframe$V117 <- ifelse(Dataframe$`V117: Private vs state ownership of business` <= -1, NA, Dataframe$`V117: Private vs state ownership of business`)

table(Dataframe$V117)

Dataframe$S22 <- Dataframe$V117

#Government responsibility

table(Dataframe$`V118: Government responsibility`)

Dataframe$V118 <- ifelse(Dataframe$`V118: Government responsibility` <= -1, NA, Dataframe$`V118: Government responsibility`)

table(Dataframe$V118)

Dataframe$S23 <- Dataframe$V118

#Competition

table(Dataframe$`V119: Competition good or harmful`)

Dataframe$V119 <- ifelse(Dataframe$`V119: Competition good or harmful` <= -1, NA, Dataframe$`V119: Competition good or harmful`)

table(Dataframe$V119)

Dataframe$S24 <- Dataframe$V119

#Democracy: Tax

table(Dataframe$`V152: Democracy: Governments tax the rich and subsidize the poor.`)

Dataframe$V152 <- ifelse(Dataframe$`V152: Democracy: Governments tax the rich and subsidize the poor.` <= -1, NA, Dataframe$`V152: Democracy: Governments tax the rich and subsidize the poor.`)

table(Dataframe$V152)

Dataframe$S25 <- Dataframe$V152

#Democracy: Unemployment

table(Dataframe$`V155: Democracy: People receive state aid for unemployment.`)

Dataframe$V155 <- ifelse(Dataframe$`V155: Democracy: People receive state aid for unemployment.` <= -1, NA, Dataframe$`V155: Democracy: People receive state aid for unemployment.`)

table(Dataframe$V155)

Dataframe$S26 <- Dataframe$V155

#INDEPENDENTES

#Classe

table(Dataframe$`V242: Profession/job`)

Dataframe$V242 <- recode(Dataframe$`V242: Profession/job`, `1` = 0, `4` = 0, `6` = 0, `10` = 0)

Dataframe$V242 <- ifelse(Dataframe$V242 <= 0, NA, Dataframe$V242)

table(Dataframe$V242)

#Religi?o

table(Dataframe$`V185b: Religious denomination - detailed list`)

Dataframe$V185 <- recode(Dataframe$`V185b: Religious denomination - detailed list`, `2e+07` = 0, `30100000` = 0, `4e+07` = 0,
                         `7e+07` = 0, `9e+07` = 0)

Dataframe$V185 <- ifelse(Dataframe$V185 <= 0, NA, Dataframe$V185)

table(Dataframe$V185)

#Partido

table(Dataframe$`V231: Which party would you vote: first choice`)

Dataframe$V231 <- recode(Dataframe$`V231: Which party would you vote: first choice`, `76005` = 0)

Dataframe$V231 <- ifelse(Dataframe$V231 <= 5, NA, Dataframe$V231)

Dataframe$V231 <- ifelse(Dataframe$V231 >= 76007, NA, Dataframe$V231)

table(Dataframe$V231)

#Partido negativo

table(Dataframe$`V233: Party that would never vote`)

Dataframe$V233 <- Dataframe$`V233: Party that would never vote`

#Comparecimento servi?os religiosos

table(Dataframe$`V186: How often do you attend religious services`)

Dataframe$V186 <- recode(Dataframe$`V186: How often do you attend religious services`, `1` = 3, `2` = 3, `3` = 2, `4` = 2, `5` = 2, `6` = 1, `7` = 1)

Dataframe$V186 <- ifelse(Dataframe$V186 <= -1, NA, Dataframe$V186)

table(Dataframe$V186)

#Ideologia

table(Dataframe$`V114: Self positioning in political scale`)

Dataframe$V114 <- ifelse(Dataframe$`V114: Self positioning in political scale` <= -1, NA, Dataframe$`V114: Self positioning in political scale`)

table(Dataframe$V114)

Dataframe2006 <- Dataframe

#WVS2014

Dataframe <- read_excel("~/Banco de Dados/WVS/Dados/F00007581-WV6_Data_Brazil_Excel_v20201117.xlsx")

#Important in life: Religion

table(Dataframe$`V9: Important in life: Religion`)

Dataframe$V9 <- recode(Dataframe$`V9: Important in life: Religion`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V9 <- ifelse(Dataframe$V9 <= -1, NA, Dataframe$V9)

table(Dataframe$V9)

Dataframe$S1 <- Dataframe$V9

#Neigh Homo

table(Dataframe$`V40: Neighbours: Homosexuals`)

Dataframe$V40 <- recode(Dataframe$`V40: Neighbours: Homosexuals`, `2` = 0)

table(Dataframe$V40)

Dataframe$S2 <- Dataframe$V40

#Believe God

table(Dataframe$`V148: Believe in: God`)

Dataframe$V148 <- recode(Dataframe$`V148: Believe in: God`, `2` = 0)

Dataframe$V148 <- ifelse(Dataframe$V148 <= -1, NA, Dataframe$V148)

table(Dataframe$V148)

#Importance of God

table(Dataframe$`V152: How important is God in your life`)

Dataframe$V152 <- ifelse(Dataframe$`V152: How important is God in your life` <= -1, NA, Dataframe$`V152: How important is God in your life`)

table(Dataframe$V152)

Dataframe$S3 <- Dataframe$V152

#Child faith

table(Dataframe$`V19: Important child qualities: religious faith`)

Dataframe$V19 <- recode(Dataframe$`V19: Important child qualities: religious faith`, `2` = 0)

Dataframe$V19 <- ifelse(Dataframe$V19 <= -1, NA, Dataframe$V19)

table(Dataframe$V19)

Dataframe$S4 <- Dataframe$V19

#Homosexuality

table(Dataframe$`V203: Justifiable: Homosexuality`)

Dataframe$V203 <- ifelse(Dataframe$`V203: Justifiable: Homosexuality` <= -1, NA, Dataframe$`V203: Justifiable: Homosexuality`)

table(Dataframe$V203)

Dataframe$S5 <- Dataframe$V203

#Abortion

table(Dataframe$`V204: Justifiable: Abortion`)

Dataframe$V204 <- ifelse(Dataframe$`V204: Justifiable: Abortion` <= -1, NA, Dataframe$`V204: Justifiable: Abortion`)

table(Dataframe$V204)

Dataframe$S6 <- Dataframe$V204

#Divorce

table(Dataframe$`V205: Justifiable: Divorce`)

Dataframe$V205 <- ifelse(Dataframe$`V205: Justifiable: Divorce` <= -1, NA, Dataframe$`V205: Justifiable: Divorce`)

table(Dataframe$V205)

Dataframe$S7 <- Dataframe$V205

#child obedience

table(Dataframe$`V21: Important child qualities: obedience`)

Dataframe$V21 <- recode(Dataframe$`V21: Important child qualities: obedience`, `2` = 0)

Dataframe$V21 <- ifelse(Dataframe$V21 <= -1, NA, Dataframe$V21)

table(Dataframe$V21)

Dataframe$S8 <- Dataframe$V21

#Aims of country

table(Dataframe$`V60: Aims of country: first choice`)

Dataframe$V60 <- ifelse(Dataframe$`V60: Aims of country: first choice` == 2, 2, 0)

table(Dataframe$V60)

table(Dataframe$`V61: Aims of country: second choice`)

Dataframe$V61 <- ifelse(Dataframe$`V61: Aims of country: second choice` == 2, 1, 0)

table(Dataframe$V61)

Dataframe$country <- Dataframe$V60 + Dataframe$V61

table(Dataframe$country)

Dataframe$S9 <- Dataframe$country

#Aims of respondent

table(Dataframe$`V62: Aims of respondent: first choice`)

Dataframe$V62 <- ifelse(Dataframe$`V62: Aims of respondent: first choice` == 1, 2, 0)

table(Dataframe$V62)

table(Dataframe$`V63: Aims of respondent: second choice`)

Dataframe$V63 <- ifelse(Dataframe$`V63: Aims of respondent: second choice` == 1, 1, 0)

table(Dataframe$V63)

Dataframe$respondent <- Dataframe$V62 + Dataframe$V63

table(Dataframe$respondent)

Dataframe$S10 <- Dataframe$respondent

#Most important

table(Dataframe$`V64: Most important: first choice`)

Dataframe$V64 <- ifelse(Dataframe$`V64: Most important: first choice` == 4, 2, 0)

table(Dataframe$V64)

table(Dataframe$`V65: Most important: second choice`)

Dataframe$V65 <- ifelse(Dataframe$`V65: Most important: second choice` == 4, 1, 0)

table(Dataframe$V65)

Dataframe$most <- Dataframe$V64 + Dataframe$V65

table(Dataframe$most)

Dataframe$S11 <- Dataframe$most

#Respect for authority

table(Dataframe$`V69: Future changes: Greater respect for authority`)

Dataframe$V69 <- recode(Dataframe$`V69: Future changes: Greater respect for authority`, `1` = 3, `3` = 1)

Dataframe$V69 <- ifelse(Dataframe$V69 <= -1, NA, Dataframe$V69)

table(Dataframe$V69)

Dataframe$S12 <- Dataframe$V69

#National pride

table(Dataframe$`V211: How proud of nationality`)

Dataframe$V211 <- recode(Dataframe$`V211: How proud of nationality`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V211 <- ifelse(Dataframe$V211 <= -1, NA, Dataframe$V211)

Dataframe$V211 <- ifelse(Dataframe$V211 >= 5, NA, Dataframe$V211)

table(Dataframe$V211)

Dataframe$S13 <- Dataframe$V211

#Political leader

table(Dataframe$`V51: Men make better political leaders than women do`)

Dataframe$V51 <- recode(Dataframe$`V51: Men make better political leaders than women do`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V51 <- ifelse(Dataframe$V51 <= -1, NA, Dataframe$V51)

table(Dataframe$V51)

Dataframe$S14 <- Dataframe$V51

#Schwart

table(Dataframe$`V79: Schwartz: It is important to this person tradition`)

Dataframe$V79 <- recode(Dataframe$`V79: Schwartz: It is important to this person tradition`, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1)

Dataframe$V79 <- ifelse(Dataframe$V79 <= -1, NA, Dataframe$V79)

table(Dataframe$V79)

#Army rule

table(Dataframe$`V129: Political system: Having the army rule`)

Dataframe$V129 <- recode(Dataframe$`V129: Political system: Having the army rule`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V129 <- ifelse(Dataframe$V129 <= -1, NA, Dataframe$V129)

table(Dataframe$V129)

Dataframe$S15 <- Dataframe$V129

#Democracy: Army

table(Dataframe$`V135: Democracy: The army takes over when government is incompetent.`)

Dataframe$V135 <- ifelse(Dataframe$`V135: Democracy: The army takes over when government is incompetent.` <= -1, NA, Dataframe$`V135: Democracy: The army takes over when government is incompetent.`)

table(Dataframe$V135)

Dataframe$S16 <- Dataframe$V135

#Jobs for man

table(Dataframe$`V45: Jobs scarce: Men should have more right to a job than women`)

Dataframe$V45 <- recode(Dataframe$`V45: Jobs scarce: Men should have more right to a job than women`, `1` = 3, `3` = 1)

Dataframe$V45 <- ifelse(Dataframe$V45 <= -1, NA, Dataframe$V45)

table(Dataframe$V45)

Dataframe$S17 <- Dataframe$V45

#Democracy: Elections

table(Dataframe$`V133: Democracy: People choose their leaders in free elections.`)

Dataframe$V133 <- ifelse(Dataframe$`V133: Democracy: People choose their leaders in free elections.` <= -1, NA, Dataframe$`V133: Democracy: People choose their leaders in free elections.`)

table(Dataframe$V133)

Dataframe$S18 <- Dataframe$V133

#Democracy: Civil rights

table(Dataframe$`V136: Democracy: Civil rights protect people’s liberty against oppression.`)

Dataframe$V136 <- ifelse(Dataframe$`V136: Democracy: Civil rights protect people’s liberty against oppression.` <= -1, NA, Dataframe$`V136: Democracy: Civil rights protect people’s liberty against oppression.`)

table(Dataframe$V136)

Dataframe$S19 <- Dataframe$V136

#Democracy: Men = Woman

table(Dataframe$`V139: Democracy: Women have the same rights as men.`)

Dataframe$V139 <- ifelse(Dataframe$`V139: Democracy: Women have the same rights as men.` <= -1, NA, Dataframe$`V139: Democracy: Women have the same rights as men.`)

table(Dataframe$V139)

Dataframe$S20 <- Dataframe$V139

#Honest elections

table(Dataframe$`V228J: Some people think that having honest elections makes a lot of difference in their lives;  other people think that it doesn’t matter much`)

Dataframe$V228 <- recode(Dataframe$`V228J: Some people think that having honest elections makes a lot of difference in their lives;  other people think that it doesn’t matter much`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$V228 <- ifelse(Dataframe$V228 <= -1, NA, Dataframe$V228)

table(Dataframe$V228)

#Income equality

table(Dataframe$`V96: Income equality`)

Dataframe$V96 <- ifelse(Dataframe$`V96: Income equality` <= -1, NA, Dataframe$`V96: Income equality`)

table(Dataframe$V96)

Dataframe$S21 <- Dataframe$V228

#Private/public ownership

table(Dataframe$`V97: Private vs state ownership of business`)

Dataframe$V97 <- ifelse(Dataframe$`V97: Private vs state ownership of business` <= -1, NA, Dataframe$`V97: Private vs state ownership of business`)

table(Dataframe$V97)

Dataframe$S22 <- Dataframe$V97

#Government responsibility

table(Dataframe$`V98: Government responsibility`)

Dataframe$V98 <- ifelse(Dataframe$`V98: Government responsibility` <= -1, NA, Dataframe$`V98: Government responsibility`)

table(Dataframe$V98)

Dataframe$S23 <- Dataframe$V98

#Competition

table(Dataframe$`V99: Competition good or harmful`)

Dataframe$V99 <- ifelse(Dataframe$`V99: Competition good or harmful` <= -1, NA, Dataframe$`V99: Competition good or harmful`)

table(Dataframe$V99)

Dataframe$S24 <- Dataframe$V99

#Democracy: Tax

table(Dataframe$`V131: Democracy: Governments tax the rich and subsidize the poor.`)

Dataframe$V131 <- ifelse(Dataframe$`V131: Democracy: Governments tax the rich and subsidize the poor.` <= -1, NA, Dataframe$`V131: Democracy: Governments tax the rich and subsidize the poor.`)

table(Dataframe$V131)

Dataframe$S25 <- Dataframe$V131

#Democracy: Unemployment

table(Dataframe$`V134: Democracy: People receive state aid for unemployment.`)

Dataframe$V134 <- ifelse(Dataframe$`V134: Democracy: People receive state aid for unemployment.` <= -1, NA, Dataframe$`V134: Democracy: People receive state aid for unemployment.`)

table(Dataframe$V134)

Dataframe$S26 <- Dataframe$V134

#INDEPENDENTES

#Religi?o

table(Dataframe$`V144: Religious denomination - detailed list`)

Dataframe$V144 <- Dataframe$`V144: Religious denomination - detailed list`

#Partido

table(Dataframe$`V228: Which party would you vote for if there were a national election tomorrow`)

Dataframe$V228 <- Dataframe$`V228: Which party would you vote for if there were a national election tomorrow`


#Comparecimento servi?os religiosos

table(Dataframe$`V145: How often do you attend religious services`)

Dataframe$V145 <- recode(Dataframe$`V145: How often do you attend religious services`, `1` = 3, `2` = 3, `3` = 2, `4` = 2, `5` = 2, `6` = 1, `7` = 1)

Dataframe$V145 <- ifelse(Dataframe$V145 <= -1, NA, Dataframe$V145)

table(Dataframe$V145)

#Ideologia

table(Dataframe$`V95: Self positioning in political scale`)

Dataframe$V95 <- ifelse(Dataframe$`V95: Self positioning in political scale` <= -1, NA, Dataframe$`V95: Self positioning in political scale`)

table(Dataframe$V95)

Dataframe2014 <- Dataframe

#WVS2018

Dataframe <- read_excel("~/Banco de Dados/WVS/Dados/F00013178-WVS_Wave_7_Brazil_Excel_v5.0.xlsx")

#Important in life: Religion

table(Dataframe$`Q6: Important in life: Religion`)

Dataframe$Q6 <- recode(Dataframe$`Q6: Important in life: Religion`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$Q6 <- ifelse(Dataframe$Q6 <= -1, NA, Dataframe$Q6)

table(Dataframe$Q6)

Dataframe$S1 <- Dataframe$Q6

#Neigh Homo

table(Dataframe$`Q22: Neighbors: Homosexuals`)

Dataframe$Q22 <- recode(Dataframe$`Q22: Neighbors: Homosexuals`, `2` = 0)

table(Dataframe$Q22)

Dataframe$S2 <- Dataframe$Q22

#Believe God

table(Dataframe$`Q165: Believe in: God`)

Dataframe$Q165 <- recode(Dataframe$`Q165: Believe in: God`, `2` = 0)

Dataframe$Q165 <- ifelse(Dataframe$Q165 <= -1, NA, Dataframe$Q165)

table(Dataframe$Q165)

#Importance of God

table(Dataframe$`Q164: Importance of God`)

Dataframe$Q164 <- ifelse(Dataframe$`Q164: Importance of God` <= -1, NA, Dataframe$`Q164: Importance of God`)

table(Dataframe$Q164)

Dataframe$S3 <- Dataframe$Q164

#Child faith

table(Dataframe$`Q15: Important child qualities: religious faith`)

Dataframe$Q15 <- recode(Dataframe$`Q15: Important child qualities: religious faith`, `2` = 0)

table(Dataframe$Q15)

Dataframe$S4 <- Dataframe$Q15

#Homosexuality

table(Dataframe$`Q182: Justifiable: Homosexuality`)

Dataframe$Q182 <- ifelse(Dataframe$`Q182: Justifiable: Homosexuality` <= -1, NA, Dataframe$`Q182: Justifiable: Homosexuality`)

table(Dataframe$Q182)

Dataframe$S5 <- Dataframe$Q182

#Abortion

table(Dataframe$`Q184: Justifiable: Abortion`)

Dataframe$Q184 <- ifelse(Dataframe$`Q184: Justifiable: Abortion` <= -1, NA, Dataframe$`Q184: Justifiable: Abortion`)

table(Dataframe$Q184)

Dataframe$S6 <- Dataframe$Q184

#Divorce

table(Dataframe$`Q185: Justifiable: Divorce`)

Dataframe$Q185 <- ifelse(Dataframe$`Q185: Justifiable: Divorce` <= -1, NA, Dataframe$`Q185: Justifiable: Divorce`)

table(Dataframe$Q185)

Dataframe$S7 <- Dataframe$Q185

#Casual Sex

table(Dataframe$`Q193: Justifiable: Having casual sex`)

Dataframe$Q193 <- ifelse(Dataframe$`Q193: Justifiable: Having casual sex` <= -1, NA, Dataframe$`Q193: Justifiable: Having casual sex`)

table(Dataframe$Q193)

#Homo Couple

table(Dataframe$`Q36: Homosexual couples are as good parents as other couples`)

Dataframe$Q36 <- recode(Dataframe$`Q36: Homosexual couples are as good parents as other couples`, `2` = 4, `4` = 2)

Dataframe$Q36 <- ifelse(Dataframe$Q36 <= -1, NA, Dataframe$Q36)

table(Dataframe$Q36)

#child obedience

table(Dataframe$`Q17: Important child qualities: obedience`)

Dataframe$Q17 <- recode(Dataframe$`Q17: Important child qualities: obedience`, `2` = 0)

table(Dataframe$Q17)

Dataframe$S8 <- Dataframe$Q17

#Societal change

table(Dataframe$`Q42: Basic kinds of attitudes concerning society`)

Dataframe$Q42 <- ifelse(Dataframe$`Q42: Basic kinds of attitudes concerning society` <= -1, NA, Dataframe$`Q42: Basic kinds of attitudes concerning society`)

table(Dataframe$Q42)

#Aims of country

table(Dataframe$`Q152: Aims of country: first choice`)

Dataframe$Q152 <- ifelse(Dataframe$`Q152: Aims of country: first choice` == 2, 2, 0)

table(Dataframe$Q152)

table(Dataframe$`Q153: Aims of country: second choice`)

Dataframe$Q153 <- ifelse(Dataframe$`Q153: Aims of country: second choice` == 2, 1, 0)

table(Dataframe$Q153)

Dataframe$country <- Dataframe$Q152 + Dataframe$Q153

table(Dataframe$country)

Dataframe$S9 <- Dataframe$country

#Aims of respondent

table(Dataframe$`Q154: Aims of respondent: first choice`)

Dataframe$Q154 <- ifelse(Dataframe$`Q154: Aims of respondent: first choice` == 1, 2, 0)

table(Dataframe$Q154)

table(Dataframe$`Q155: Aims of respondent: second choice`)

Dataframe$Q155 <- ifelse(Dataframe$`Q155: Aims of respondent: second choice` == 1, 1, 0)

table(Dataframe$Q155)

Dataframe$respondent <- Dataframe$Q154 + Dataframe$Q155

table(Dataframe$respondent)

Dataframe$S10 <- Dataframe$respondent

#Most important

table(Dataframe$`Q156: Most important: first choice`)

Dataframe$Q156 <- ifelse(Dataframe$`Q156: Most important: first choice` == 4, 2, 0)

table(Dataframe$Q156)

table(Dataframe$`Q157: Most important: second choice`)

Dataframe$Q157 <- ifelse(Dataframe$`Q157: Most important: second choice` == 4, 1, 0)

table(Dataframe$Q157)

Dataframe$most <- Dataframe$Q156 + Dataframe$Q157

table(Dataframe$most)

Dataframe$S11 <- Dataframe$most

#Respect for authority

table(Dataframe$`Q45: Future changes: Greater respect for authority`)

Dataframe$Q45 <- recode(Dataframe$`Q45: Future changes: Greater respect for authority`, `1` = 3, `3` = 1)

Dataframe$Q45 <- ifelse(Dataframe$Q45 <= -1, NA, Dataframe$Q45)

table(Dataframe$Q45)

Dataframe$S12 <- Dataframe$Q45

#National pride

table(Dataframe$`Q254: National pride`)

Dataframe$Q254 <- recode(Dataframe$`Q254: National pride`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$Q254 <- ifelse(Dataframe$Q254 <= -1, NA, Dataframe$Q254)

Dataframe$Q254 <- ifelse(Dataframe$Q254 >= 5, NA, Dataframe$Q254)

table(Dataframe$Q254)

Dataframe$S13 <- Dataframe$Q254

#Political leader

table(Dataframe$`Q29: Men make better political leaders than women do`)

Dataframe$Q29 <- recode(Dataframe$`Q29: Men make better political leaders than women do`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$Q29 <- ifelse(Dataframe$Q29 <= -1, NA, Dataframe$Q29)

table(Dataframe$Q29)

Dataframe$S14 <- Dataframe$Q29

#Death penalty

table(Dataframe$`Q195: Justifiable: Death penalty`)

Dataframe$Q195 <- ifelse(Dataframe$`Q195: Justifiable: Death penalty` <= -1, NA, Dataframe$`Q195: Justifiable: Death penalty`)

table(Dataframe$Q195)

#Army rule

table(Dataframe$`Q237: Political system: Having the army rule`)

Dataframe$Q237 <- recode(Dataframe$`Q237: Political system: Having the army rule`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$Q237 <- ifelse(Dataframe$Q237 <= -1, NA, Dataframe$Q237)

table(Dataframe$Q237)

Dataframe$S15 <- Dataframe$Q237

#Democracy: Army

table(Dataframe$`Q245: Democracy: The army takes over when government is incompetent`)

Dataframe$Q245 <- ifelse(Dataframe$`Q245: Democracy: The army takes over when government is incompetent` <= -1, NA, Dataframe$`Q245: Democracy: The army takes over when government is incompetent`)

table(Dataframe$Q245)

Dataframe$S16 <- Dataframe$Q245

#Job scarce

table(Dataframe$`Q33: Jobs scarce: Men should have more right to a job than women`)

Dataframe$Q33 <- recode(Dataframe$`Q33: Jobs scarce: Men should have more right to a job than women`, `2` = 4, `4` = 2)

Dataframe$Q33 <- ifelse(Dataframe$Q33 <= -1, NA, Dataframe$Q33)

table(Dataframe$Q33)

Dataframe$S17 <- Dataframe$Q33

#Democracy: Elections

table(Dataframe$`Q243: Democracy: People choose their leaders in free elections`)

Dataframe$Q243 <- ifelse(Dataframe$`Q243: Democracy: People choose their leaders in free elections` <= -1, NA, Dataframe$`Q243: Democracy: People choose their leaders in free elections`)

table(Dataframe$Q243)

Dataframe$S18 <- Dataframe$Q243

#Democracy: Civil rights

table(Dataframe$`Q246: Democracy: Civil rights protect people’s liberty against oppression`)

Dataframe$Q246 <- ifelse(Dataframe$`Q246: Democracy: Civil rights protect people’s liberty against oppression` <= -1, NA, Dataframe$`Q246: Democracy: Civil rights protect people’s liberty against oppression`)

table(Dataframe$Q246)
Dataframe$S19 <- Dataframe$Q246

table(Dataframe$`Q249: Democracy: Women have the same rights as men`)

Dataframe$Q249 <- ifelse(Dataframe$`Q249: Democracy: Women have the same rights as men` <= -1, NA, Dataframe$`Q249: Democracy: Women have the same rights as men`)

table(Dataframe$Q249)

Dataframe$S20 <- Dataframe$Q249

#Honest elections

table(Dataframe$`Q234: Some people think that having honest elections makes a lot of difference in their lives;  other people think that it doesn’t matter much`)

Dataframe$Q234 <- recode(Dataframe$`Q234: Some people think that having honest elections makes a lot of difference in their lives;  other people think that it doesn’t matter much`, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

Dataframe$Q234 <- ifelse(Dataframe$Q234 <= -1, NA, Dataframe$Q234)

table(Dataframe$Q234)

#Income equality

table(Dataframe$`Q106: Incomes should be made more equal vs There should be greater incentives for individual effort`)

Dataframe$Q106 <- ifelse(Dataframe$`Q106: Incomes should be made more equal vs There should be greater incentives for individual effort` <= -1, NA, Dataframe$`Q106: Incomes should be made more equal vs There should be greater incentives for individual effort`)

table(Dataframe$Q106)

Dataframe$S21 <- Dataframe$Q106

#Private/public ownership

table(Dataframe$`Q107: Private vs state ownership of business`)

Dataframe$Q107 <- ifelse(Dataframe$`Q107: Private vs state ownership of business` <= -1, NA, Dataframe$`Q107: Private vs state ownership of business`)

table(Dataframe$Q107)

Dataframe$S22 <- Dataframe$Q107

#Government responsibility

table(Dataframe$`Q108: Government's vs individual's responsibility`)

Dataframe$Q108 <- ifelse(Dataframe$`Q108: Government's vs individual's responsibility` <= -1, NA, Dataframe$`Q108: Government's vs individual's responsibility`)

table(Dataframe$Q108)

Dataframe$S23 <- Dataframe$Q108

#Competition

table(Dataframe$`Q109: Competition good or harmful`)

Dataframe$Q109 <- ifelse(Dataframe$`Q109: Competition good or harmful` <= -1, NA, Dataframe$`Q109: Competition good or harmful`)

table(Dataframe$Q109)

Dataframe$S24 <- Dataframe$Q109

#Democracy: Tax

table(Dataframe$`Q241: Democracy: Governments tax the rich and subsidize the poor`)

Dataframe$Q241 <- ifelse(Dataframe$`Q241: Democracy: Governments tax the rich and subsidize the poor` <= -1, NA, Dataframe$`Q241: Democracy: Governments tax the rich and subsidize the poor`)

table(Dataframe$Q241)

Dataframe$S25 <- Dataframe$Q241

#Democracy: Unemployment

table(Dataframe$`Q244: Democracy: People receive state aid for unemployment`)

Dataframe$Q244 <- ifelse(Dataframe$`Q244: Democracy: People receive state aid for unemployment` <= -1, NA, Dataframe$`Q244: Democracy: People receive state aid for unemployment`)

table(Dataframe$Q244)

Dataframe$S26 <- Dataframe$Q244

#INDEPENDENTES

#Classe

table(Dataframe$`Q281: Respondent - Occupational group`)

Dataframe$Q281 <- recode(Dataframe$`Q281: Respondent - Occupational group`, `0` = 13, `10` = 0)

Dataframe$Q281 <- ifelse(Dataframe$Q281 <= 0, NA, Dataframe$Q281)

table(Dataframe$Q281)

#Religi?o

table(Dataframe$`Q289: Religious denominations - major groups`)

Dataframe$Q289 <- Dataframe$`Q289: Religious denominations - major groups`

#Partido

table(Dataframe$`Q223: Which party would you vote for if there were a national election tomorrow`)

Dataframe$Q223 <- Dataframe$`Q223: Which party would you vote for if there were a national election tomorrow`


#Comparecimento servi?os religiosos

table(Dataframe$`Q171: How often do you attend religious services`)

Dataframe$Q171 <- recode(Dataframe$`Q171: How often do you attend religious services`, `1` = 3, `2` = 3, `3` = 2, `4` = 2, `5` = 2, `6` = 1, `7` = 1)

Dataframe$Q171 <- ifelse(Dataframe$Q171 <= -1, NA, Dataframe$Q171)

table(Dataframe$Q171)

#Ideologia

table(Dataframe$`Q240: Left-right political scale`)

Dataframe$Q240 <- ifelse(Dataframe$`Q240: Left-right political scale` <= -1, NA, Dataframe$`Q240: Left-right political scale`)

table(Dataframe$Q240)

Dataframe2018 <- Dataframe

#ACDD2019

Dataframe <- read_sav("~/Banco de Dados/ACDD/bdacara19.sav")

Dataframe_numeric <- lapply(Dataframe, as.numeric)

Dataframe <- as.data.frame(Dataframe_numeric)

#Aborto

table(Dataframe$temas7)

Dataframe$V107 <- recode(Dataframe$temas7, `1` = 3, `2` = 1, `3` = 2)

Dataframe$V107 <- ifelse(Dataframe$V107 >= 50, NA, Dataframe$V107)

table(Dataframe$V107)

#Casamento gay

table(Dataframe$temas2)

Dataframe$V102 <- recode(Dataframe$temas2, `1` = 3, `2` = 1, `3` = 2)

Dataframe$V102 <- ifelse(Dataframe$V102 >= 50, NA, Dataframe$V102)

table(Dataframe$V102)

#Ado??o gay

table(Dataframe$temas3)

Dataframe$V103 <- recode(Dataframe$temas3, `1` = 3, `2` = 1, `3` = 2)

Dataframe$V103 <- ifelse(Dataframe$V103 >= 50, NA, Dataframe$V103)

table(Dataframe$V103)

#Drogas

table(Dataframe$temas5)

Dataframe$V105 <- recode(Dataframe$temas5, `1` = 3, `2` = 1, `3` = 2)

Dataframe$V105 <- ifelse(Dataframe$V105 >= 50, NA, Dataframe$V105)

table(Dataframe$V105)

#Ensino religioso

table(Dataframe$temas10)

Dataframe$V110 <- recode(Dataframe$temas10, `1` = 3, `2` = 1, `3` = 2)

Dataframe$V110 <- ifelse(Dataframe$V110 >= 50, NA, Dataframe$V110)

table(Dataframe$V110)

#Pena de morte

table(Dataframe$temas4)

Dataframe$V104 <- recode(Dataframe$temas4, `1` = 3, `2` = 1, `3` = 2)

Dataframe$V104 <- ifelse(Dataframe$V104 >= 50, NA, Dataframe$V104)

table(Dataframe$V104)

#Golpe

table(Dataframe$jg1)

Dataframe$V4 <- recode(Dataframe$jg1, `2` = 0)

Dataframe$V4 <- ifelse(Dataframe$V4 >= 50, NA, Dataframe$V4)

table(Dataframe$V4)

table(Dataframe$jg2)

Dataframe$V5 <- recode(Dataframe$jg2, `2` = 0)

Dataframe$V5 <- ifelse(Dataframe$V5 >= 50, NA, Dataframe$V5)

table(Dataframe$V5)

table(Dataframe$jg3)

Dataframe$V6 <- recode(Dataframe$jg3, `2` = 0)

Dataframe$V6 <- ifelse(Dataframe$V6 >= 50, NA, Dataframe$V6)

table(Dataframe$V6)

table(Dataframe$jg4)

Dataframe$V7 <- recode(Dataframe$jg4, `2` = 0)

Dataframe$V7 <- ifelse(Dataframe$V7 >= 50, NA, Dataframe$V7)

table(Dataframe$V7)

table(Dataframe$jg5)

Dataframe$V8 <- recode(Dataframe$jg5, `2` = 0)

Dataframe$V8 <- ifelse(Dataframe$V8 >= 50, NA, Dataframe$V8)

table(Dataframe$V8)

table(Dataframe$jg6)

Dataframe$V9 <- recode(Dataframe$jg6, `2` = 0)

Dataframe$V9 <- ifelse(Dataframe$V9 >= 50, NA, Dataframe$V9)

table(Dataframe$V9)

#Fechar congresso

table(Dataframe$fechacn)

Dataframe$V10 <- recode(Dataframe$fechacn, `2` = 0)

Dataframe$V10 <- ifelse(Dataframe$V10 >= 50, NA, Dataframe$V10)

table(Dataframe$V10)

#Maioridade penal

table(Dataframe$temas1)

Dataframe$V101 <- recode(Dataframe$temas1, `1` = 3, `2` = 1, `3` = 2)

Dataframe$V101 <- ifelse(Dataframe$V101 >= 50, NA, Dataframe$V101)

table(Dataframe$V101)

#Cotas

table(Dataframe$temas6)

Dataframe$V106 <- recode(Dataframe$temas6, `1` = 3, `2` = 1, `3` = 2)

Dataframe$V106 <- ifelse(Dataframe$V106 >= 50, NA, Dataframe$V106)

table(Dataframe$V106)

#Democracia

table(Dataframe$prefdem)

Dataframe$V3 <- recode(Dataframe$prefdem, `1` = 3, `3` = 1)

Dataframe$V3 <- ifelse(Dataframe$V3 >= 50, NA, Dataframe$V3)

table(Dataframe$V3)

#Dem sem partidos

table(Dataframe$spart)

Dataframe$V34 <- recode(Dataframe$spart, `2` = 0)

Dataframe$V34 <- ifelse(Dataframe$V34 >= 3, NA, Dataframe$V34)

table(Dataframe$V34)

#Cotas

table(Dataframe$temas9)

Dataframe$V109 <- recode(Dataframe$temas9, `1` = 3, `2` = 1, `3` = 2)

Dataframe$V109 <- ifelse(Dataframe$V109 >= 50, NA, Dataframe$V109)

table(Dataframe$V109)

#Equidade

table(Dataframe$papelest3)

Dataframe$V115 <- ifelse(Dataframe$papelest3 >= 50, NA, Dataframe$papelest3)

table(Dataframe$V115)

#Empresas

table(Dataframe$papelest1)

Dataframe$V113 <- ifelse(Dataframe$papelest1 >= 50, NA, Dataframe$papelest1)

table(Dataframe$V113)

#Responsabilidade gov

table(Dataframe$papelest2)

Dataframe$V114 <- ifelse(Dataframe$papelest2 >= 50, NA, Dataframe$papelest2)

table(Dataframe$V114)

#Economia

table(Dataframe$econger)

Dataframe$V112 <- ifelse(Dataframe$econger >= 50, NA, Dataframe$econger)

table(Dataframe$V112)

#INDEPENDENTES

#Classe

table(Dataframe$sit)

Dataframe$sit <- recode(Dataframe$sit, `5` = 0, `7` = 0, `8` = 0, `9` = 0,
                        `10` = 0)

Dataframe$sit <- ifelse(Dataframe$sit <= 0, NA, Dataframe$sit)

Dataframe$sit <- recode(Dataframe$sit, `6` = 1, `4` = 2, `1` = 3, `2` = 4, `3` = 5)

table(Dataframe$sit)

#Religi?o

table(Dataframe$relig)

#Partido

table(Dataframe$partsimp2)

#Partido negativo

table(Dataframe$partsimpn2)

#Comparecimento servi?os religiosos

table(Dataframe$freqigreja)

Dataframe$V141 <- recode(Dataframe$freqigreja, `1` = 3, `2` = 3, `3` = 2, `4` = 2, `5` = 1, `6` = 1)

Dataframe$V141 <- ifelse(Dataframe$V141 >= 99, NA, Dataframe$V141)

table(Dataframe$V141)

#Ideologia

table(Dataframe$ed)

Dataframe$ed <- ifelse(Dataframe$ed >= 88, NA, Dataframe$ed)

table(Dataframe$ed)

Dataframe2019 <- Dataframe

#CLIVAGENS2023

Dataframe <- read_csv("~/Banco de Dados/JULIAN CLIVAGENS/df.csv")

#Aborto

table(Dataframe$P47)

Dataframe$P47 <- ifelse(Dataframe$P47 >= 11, NA, Dataframe$P47)

table(Dataframe$P47)

#Casamento gay

table(Dataframe$P40)

Dataframe$P40 <- ifelse(Dataframe$P40 >= 11, NA, Dataframe$P40)

table(Dataframe$P40)

#Drogas

table(Dataframe$P43)

Dataframe$P43 <- ifelse(Dataframe$P43 >= 11, NA, Dataframe$P43)

table(Dataframe$P43)

#Pena de morte

table(Dataframe$P42)

Dataframe$P42 <- ifelse(Dataframe$P42 >= 11, NA, Dataframe$P42)

table(Dataframe$P42)

#Fechar congresso

table(Dataframe$P51)

Dataframe$P51 <- ifelse(Dataframe$P51 >= 11, NA, Dataframe$P51)

table(Dataframe$P51)

#Maioridade

table(Dataframe$P41)

Dataframe$P41 <- ifelse(Dataframe$P41 >= 11, NA, Dataframe$P41)

table(Dataframe$P41)

#Armas

table(Dataframe$P44)

Dataframe$P44 <- ifelse(Dataframe$P44 >= 11, NA, Dataframe$P44)

table(Dataframe$P44)

#Democracia

table(Dataframe$P50)

Dataframe$P50 <- recode(Dataframe$P50, `1` = 2, `2` = 3, `3` = 1)

Dataframe$P50 <- ifelse(Dataframe$P50 >= 4, NA, Dataframe$P50)

table(Dataframe$P50)

#Cotas

table(Dataframe$P45)

Dataframe$P45 <- ifelse(Dataframe$P45 >= 11, NA, Dataframe$P45)

table(Dataframe$P45)

#Responsabilidade gov

table(Dataframe$P35)

Dataframe$P35 <- ifelse(Dataframe$P35 >= 3, NA, Dataframe$P35)

table(Dataframe$P35)

#Desigualdades

table(Dataframe$P37)

Dataframe$P37 <- ifelse(Dataframe$P37 >= 11, NA, Dataframe$P37)

table(Dataframe$P37)

#Empresas

table(Dataframe$P36)

Dataframe$P36 <- ifelse(Dataframe$P36 >= 11, NA, Dataframe$P36)

table(Dataframe$P36)

#Classe
table(Dataframe$P10)

#Anti-PT

table(Dataframe$P18)

Dataframe$P18 <- ifelse(Dataframe$P18 <= 3, 1, 0)

table(Dataframe$P18)

#Anti-PSL
Dataframe$P20 <- ifelse(Dataframe$P20 <= 3, 1, 0)

table(Dataframe$P20)

#Attend

Dataframe$P14 <- recode(Dataframe$P14, `1` = 3, `2` = 3, `3` = 2, `4` = 2, `5` = 1, `6` = 1)

Dataframe$P14 <- ifelse(Dataframe$P14 >= 7, NA, Dataframe$P14)

Dataframe2023 <- Dataframe

####ANÁLISES FATORIAIS####

#WVS1997

df1997 <- Dataframe1997[, c("V9","V60","V183","V190","V95","V22",
                        "V197","V199","V200","V125","V126","V127","V128",
                        "V221","V179","V210","V212","V181","V123")]

fa1997 <- fa(df1997[,1:13], nfactors = 1, rotate = "varimax")

fa1997

#WVS2006

df2006 <- Dataframe2006[, c("V9","V38","V192","V19","V202",
                        "V204","V205","V116","V117",
                        "V118","V119","V152","V155","V242",
                        "V185","V231","V233","V186","V114")]

fa2006 <- fa(df2006[,1:11], nfactors = 1, rotate = "varimax")

fa2006

#WVS2014

df2014 <- Dataframe2014[, c("V9","V40","V148","V152","V19","V203","V204","V205","V96",
                        "V97","V98","V99","V131","V134","V144","V228","V145","V95")]

fa2014 <- fa(df2014[,1:12], nfactors = 1, rotate = "varimax")

fa2014

#WVS2018

df2018 <- Dataframe2018[, c("Q6","Q22","Q165","Q164","Q15","Q182","Q184","Q185",
                            "Q193","Q36","Q106","Q107","Q108","Q109","Q241","Q244","Q281",
                        "Q289","Q223","Q171","Q240")]

fa2018 <- fa(df2018[,1:14], nfactors = 1, rotate = "varimax")

fa2018

#ACDD2019

df2019 <- Dataframe2019[, c("V107","V102","V103","V105","V110","V115","V113","V114","V112",
                        "sit","relig","partsimp2","partsimpn2","V141","ed","esc")]

fa2019 <- fa(df2019[,1:9], nfactors = 2, rotate = "varimax")

fa2019

fa2019_mixed <- fa(df2019[,1:9], nfactors = 2, rotate = "varimax", cor = "mixed")

fa2019_mixed


#CLIVAGENS2023

df2023 <- Dataframe2023[, c("P47","P40","P43",
                        "P35","P37","P36","P10","P13","P17","P18","P20","P14","P12")]

fa2023 <- fa(df2023[,1:6], nfactors = 2, rotate = "varimax")

fa2023

####LONGITUDINAL####

#WVS1997

df1997$L1 <- df1997$V9
df1997$L2 <- df1997$V60
df1997$L3 <- df1997$V190
df1997$L4 <- df1997$V22
df1997$L5 <- df1997$V197
df1997$L6 <- df1997$V199
df1997$L7 <- df1997$V200
df1997$L8 <- df1997$V125
df1997$L9 <- df1997$V126
df1997$L10 <- df1997$V127
df1997$L11 <- df1997$V128

df1997long <- df1997[, c("L1","L2","L3","L4","L5","L6","L7","L8","L9","L10","L11")]

#WVS2006

df2006$L1 <- df2006$V9
df2006$L2 <- df2006$V38
df2006$L3 <- df2006$V192
df2006$L4 <- df2006$V19
df2006$L5 <- df2006$V202
df2006$L6 <- df2006$V204
df2006$L7 <- df2006$V205
df2006$L8 <- df2006$V116
df2006$L9 <- df2006$V117
df2006$L10 <- df2006$V118
df2006$L11 <- df2006$V119

df2006long <- df2006[, c("L1","L2","L3","L4","L5","L6","L7","L8","L9","L10","L11")]

#WVS2014

df2014$L1 <- df2014$V9
df2014$L2 <- df2014$V40
df2014$L3 <- df2014$V152
df2014$L4 <- df2014$V19
df2014$L5 <- df2014$V203
df2014$L6 <- df2014$V204
df2014$L7 <- df2014$V205
df2014$L8 <- df2014$V96
df2014$L9 <- df2014$V97
df2014$L10 <- df2014$V98
df2014$L11 <- df2014$V99

df2014long <- df2014[, c("L1","L2","L3","L4","L5","L6","L7","L8","L9","L10","L11")]

#WVS2018

df2018$L1 <- df2018$Q6
df2018$L2 <- df2018$Q22
df2018$L3 <- df2018$Q164
df2018$L4 <- df2018$Q15
df2018$L5 <- df2018$Q182
df2018$L6 <- df2018$Q184
df2018$L7 <- df2018$Q185
df2018$L8 <- df2018$Q106
df2018$L9 <- df2018$Q107
df2018$L10 <- df2018$Q108
df2018$L11 <- df2018$Q109

df2018long <- df2018[, c("L1","L2","L3","L4","L5","L6","L7","L8","L9","L10","L11")]

#Merge

dataframeslong <- list(
  df1997long = df1997long,
  df2006long = df2006long,
  df2014long = df2014long,
  df2018long = df2018long
)

combined_long <- bind_rows(
  .id = "ano",
  dataframeslong
)

#Análise Fatorial

fa_long <- fa(combined_long[,2:12], nfactors = 1, rotate = "varimax")

fa_long

scores_long <- factor.scores(combined_long[,2:12],fa_long)
scores_long <- scores_long$scores
combined_long <- cbind(combined_long,scores_long)

#Ajuste direção

table(combined_long$MR1,combined_long$L6) #Como visto nessa tabela de refer?ncia cruzada,
                                          #um valor menor de MR1 est? associado com um valor menor
                                          #de aceita??o ao aborto. Ent?o um valor menor de MR1
                                          #significa mais fundamentalismo. Por isso, invertemos.

combined_long$MR1 <- combined_long$MR1 * (-1)

#Wisorization

combined_long$dados_winsorized <- Winsorize(combined_long$MR1, probs = c(0.05, 1), na.rm = TRUE)

par(mfrow = c(1, 2))
hist(combined_long$MR1, main = "Dados sem winzorização", xlab = "Fundamentalismo", ylab = "Frequência",  col = "lightblue", na.rm = TRUE)
hist(combined_long$dados_winsorized, main = "Dados com winzorização", xlab = "Fundamentalismo", ylab = "", col = "lightgreen", na.rm = TRUE)


#Criando modelo de regressão longitudinal

summary(combined_long)
combined_long$anof <- as.factor(combined_long$ano)
combined_long$anof <- relevel(combined_long$anof, "df2014long")
model3 <- lm(dados_winsorized~anof, data=combined_long)
combined_long$anof <- relevel(combined_long$anof, "df2006long")
model2 <- lm(dados_winsorized~anof, data=combined_long)
combined_long$anof <- relevel(combined_long$anof, "df1997long")
model1 <- lm(dados_winsorized~anof, data=combined_long)

tab_model(model1, model2,model3, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

se_model1 <- coeftest(model1, vcov = vcovHC(model1, type = "HC0"))[, "Std. Error"]

results_df <- data.frame(
  anof = factor(c("1997", "2006", "2014","2018")),
  estimate = coef(model1),
  se = se_model1
)

figurelong1 <- ggplot(results_df, aes(x = anof, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), width = 0.2) +
  labs(y = "Fundamentalismo (Estimates)", x = "Onda") +
  theme_bw() +
  scale_x_discrete(labels = c("1997", "2006", "2014","2018"))

figurelong2 <- combined_long %>%
  ggplot(aes(x = dados_winsorized)) +
  ggdist::stat_halfeye(adjust = 0.5, justification = -.2, .width = 0, point_colour = NA) +
  geom_boxplot(width = -.12, outlier.colour = NA, alpha = 0.5, fill = "#666666") +
  facet_wrap(~ anof, labeller = as_labeller(c("df1997long" = "1997", "df2006long" = "2006", "df2014long" = "2014", "df2018long" = "2018"))) +
  theme_tq() +
  labs(title = "",
       subtitle = "",
       x = "",
       y = "",
       fill = "") +
  theme(strip.background = element_rect(fill = "#333333"))

grid.arrange(figurelong1, figurelong2, ncol = 2)

#Assumptions longitudinal

par(mfrow = c(2, 2))
plot(model1)

#Sem winzorizing e erro padrão robusto

summary(combined_long)
combined_long$anof <- as.factor(combined_long$ano)
combined_long$anof <- relevel(combined_long$anof, "df2014long")
model3 <- lm(MR1~anof, data=combined_long)
combined_long$anof <- relevel(combined_long$anof, "df2006long")
model2 <- lm(MR1~anof, data=combined_long)
combined_long$anof <- relevel(combined_long$anof, "df1997long")
model1 <- lm(MR1~anof, data=combined_long)

tab_model(model1, model2,model3, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

results_df <- data.frame(
  anof = factor(c("1997", "2006", "2014","2018")),
  estimate = coef(model1),
  se = summary(model1)$coef[, "Std. Error"]
)

figurelong1 <- ggplot(results_df, aes(x = anof, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), width = 0.2) +
  labs(y = "Fundamentalismo (Estimates)", x = "Onda") +
  theme_bw() +
  scale_x_discrete(labels = c("1997", "2006", "2014","2018"))

figurelong2 <- combined_long %>%
  ggplot(aes(x = MR1)) +
  ggdist::stat_halfeye(adjust = 0.5, justification = -.2, .width = 0, point_colour = NA) +
  geom_boxplot(width = -.12, outlier.colour = NA, alpha = 0.5, fill = "#666666") +
  facet_wrap(~ anof, labeller = as_labeller(c("df1997long" = "1997", "df2006long" = "2006", "df2014long" = "2014", "df2018long" = "2018"))) +
  theme_tq() +
  labs(title = "",
       subtitle = "",
       x = "",
       y = "",
       fill = "") +
  theme(strip.background = element_rect(fill = "#333333"))

grid.arrange(figurelong1, figurelong2, ncol = 2)

par(mfrow = c(2, 2))
plot(model1)

####Regressoes####

#WVS1997

table(Dataframe1997$V221)

Dataframe1997$R0 <- recode(Dataframe1997$`V221: Profession/job`, `1` = 10, `3` = 1, `4` = 1, `6` = 1)

Dataframe1997$R0 <- ifelse(Dataframe1997$R0 == 1, 1, 0)

table(Dataframe1997$R0)

table(Dataframe1997$`v179: Religious denomination - detailed list`)

Dataframe1997$R1 <- ifelse(Dataframe1997$`v179: Religious denomination - detailed list` == 80502000, 1, 0)

table(Dataframe1997$R1)

Dataframe1997$R2 <- Dataframe1997$V181

table(Dataframe1997$`V210: Which party would you vote for: First choice`)

Dataframe1997$R3 <- ifelse(Dataframe1997$`V210: Which party would you vote for: First choice` == 76002, 1, 0)

table(Dataframe1997$R3)

table(Dataframe1997$`V212: Party that would never vote`)

Dataframe1997$R4 <- ifelse(Dataframe1997$`V212: Party that would never vote` == 76002, 1, 0)

table(Dataframe1997$R4)

Dataframe1997$R5 <- Dataframe1997$V123

table(Dataframe1997$`V217: Highest educational level attained`)

Dataframe1997$R6 <- Dataframe1997$`V217: Highest educational level attained`

table(Dataframe1997$`V232: Size of town`)

Dataframe1997$R7 <- Dataframe1997$`V232: Size of town`

table(Dataframe1997$`V233: Ethnic group`)

Dataframe1997$R8 <- ifelse(Dataframe1997$`V233: Ethnic group` == 76001, 1, 0)

table(Dataframe1997$R8)

table(Dataframe1997$`V214: Sex`)

Dataframe1997$R9 <- Dataframe1997$`V214: Sex`

table(Dataframe1997$`V216: Age`)

Dataframe1997$R10 <- Dataframe1997$`V216: Age`

table(Dataframe1997$`V227CS: Income (CS)`)

Dataframe1997$R11 <- ifelse(Dataframe1997$`V227CS: Income (CS)` <= -1, NA, Dataframe1997$`V227CS: Income (CS)`)


df1997reg <- Dataframe1997[, c("V9","V60","V183","V190","V95","V22",
                        "V197","V199","V200","V125","V126","V127","V128",
                        "R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11","R0")]


fa1997reg <- fa(df1997reg[,1:13], nfactors = 1, rotate = "varimax")

scores_df1997reg <- factor.scores(df1997reg[,1:13],fa1997reg)
scores_df1997reg <- scores_df1997reg$scores
df1997reg <- cbind(df1997reg,scores_df1997reg)

modelo1997reg <- lm(MR1~R1+R2+R3+R4+R5+R6+R7+R8+R9+R10+R11, data=df1997reg)

df1997reg$dados_winsorized <- Winsorize(df1997reg$MR1, probs = c(0.05, 1), na.rm = TRUE)

#WVS2006

table(Dataframe2006$V242)

Dataframe2006$R0 <- recode(Dataframe2006$`V242: Profession/job`, `1` = 10, `3` = 1, `5` = 1)

Dataframe2006$R0 <- ifelse(Dataframe2006$R0 == 1, 1, 0)

table(Dataframe2006$R0)

table(Dataframe2006$`V185b: Religious denomination - detailed list`)

Dataframe2006$R1 <- ifelse(Dataframe2006$`V185b: Religious denomination - detailed list` == 80400035, 1, 0)

table(Dataframe2006$R1)

Dataframe2006$R2 <- Dataframe2006$V186

table(Dataframe2006$`V231: Which party would you vote: first choice`)

Dataframe2006$R3 <- ifelse(Dataframe2006$`V231: Which party would you vote: first choice` == 76002, 1, 0)

table(Dataframe2006$R3)

table(Dataframe2006$`V233: Party that would never vote`)

Dataframe2006$R4 <- ifelse(Dataframe2006$`V233: Party that would never vote` == 76002, 1, 0)

table(Dataframe2006$R4)

Dataframe2006$R5 <- Dataframe2006$V114

table(Dataframe2006$`V238: Highest educational level: Respondent`)

Dataframe2006$R6 <- ifelse(Dataframe2006$`V238: Highest educational level: Respondent` <= -1, NA, Dataframe2006$`V238: Highest educational level: Respondent`)

table(Dataframe2006$`V255: Size of town`)

Dataframe2006$R7 <- Dataframe2006$`V255: Size of town`

table(Dataframe2006$`V256: Ethnic group`)

Dataframe2006$R8 <- ifelse(Dataframe2006$`V256: Ethnic group` == 1400, 1, 0)

table(Dataframe2006$R8)

table(Dataframe2006$`V235: Sex`)

Dataframe2006$R9 <- Dataframe2006$`V235: Sex`

table(Dataframe2006$`V237: Age`)

Dataframe2006$R10 <- ifelse(Dataframe2006$`V237: Age` <= -1, NA, Dataframe2006$`V237: Age`)

table(Dataframe2006$`V253: Scale of incomes`)

Dataframe2006$R11 <- ifelse(Dataframe2006$`V253: Scale of incomes` <= -1, NA, Dataframe2006$`V253: Scale of incomes`)

df2006reg <- Dataframe2006[, c("V9","V38","V192","V19","V202",
                        "V204","V205","V116","V117",
                        "V118","V119",
                        "R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11","R0")]


fa2006reg <- fa(df2006reg[,1:11], nfactors = 1, rotate = "varimax")

scores_df2006reg <- factor.scores(df2006reg[,1:11],fa2006reg)
scores_df2006reg <- scores_df2006reg$scores
df2006reg <- cbind(df2006reg,scores_df2006reg)

modelo2006reg <- lm(MR1~R1+R2+R3+R4+R5+R6+R7+R8+R9+R10+R11, data=df2006reg)

df2006reg$dados_winsorized <- Winsorize(df2006reg$MR1, probs = c(0.05, 1), na.rm = TRUE)

#WVS2014

table(Dataframe2014$`V144: Religious denomination - detailed list`)

Dataframe2014$R1 <- ifelse(Dataframe2014$`V144: Religious denomination - detailed list` == 80400035, 1, 0)

table(Dataframe2014$R1)

Dataframe2014$R2 <- Dataframe2014$V145

table(Dataframe2014$`V228: Which party would you vote for if there were a national election tomorrow`)

Dataframe2014$R3 <- ifelse(Dataframe2014$`V228: Which party would you vote for if there were a national election tomorrow` == 76002, 1, 0)

table(Dataframe2014$R3)

Dataframe2014$R5 <- Dataframe2014$V95

table(Dataframe2014$`V248: Highest educational level attained`)

Dataframe2014$R6 <- ifelse(Dataframe2014$`V248: Highest educational level attained` <= -1, NA, Dataframe2014$`V248: Highest educational level attained`)

table(Dataframe2014$`V253: Size of town`)

Dataframe2014$R7 <- ifelse(Dataframe2014$`V253: Size of town` <= -1, NA, Dataframe2014$`V253: Size of town`)

table(Dataframe2014$`V254: Ethnic group`)

Dataframe2014$R8 <- ifelse(Dataframe2014$`V254: Ethnic group` == 1400, 1, 0)

table(Dataframe2014$R8)

table(Dataframe2014$`V240: Sex`)

Dataframe2014$R9 <- Dataframe2014$`V240: Sex`

table(Dataframe2014$`V242: Age`)

Dataframe2014$R10 <- ifelse(Dataframe2014$`V242: Age` <= -1, NA, Dataframe2014$`V242: Age`)

table(Dataframe2014$`V239: Scale of incomes`)

Dataframe2014$R11 <- ifelse(Dataframe2014$`V239: Scale of incomes` <= -1, NA, Dataframe2014$`V239: Scale of incomes`)

df2014reg <- Dataframe2014[, c("V9","V40","V148","V152","V19","V203","V204","V205","V96",
                        "V97","V98","V99",
                        "R1","R2","R3","R5","R6","R7","R8","R9","R10","R11")]

fa2014reg <- fa(df2014reg[,1:12], nfactors = 1, rotate = "varimax")

scores_df2014reg <- factor.scores(df2014reg[,1:12],fa2014reg)
scores_df2014reg <- scores_df2014reg$scores
df2014reg <- cbind(df2014reg,scores_df2014reg)

modelo2014reg <- lm(MR1~R1+R2+R3+R5+R6+R7+R8+R9+R10+R11, data=df2014reg)

df2014reg$dados_winsorized <- Winsorize(df2014reg$MR1, probs = c(0.05, 1), na.rm = TRUE)

#WVS2018

table(Dataframe2018$Q281)

Dataframe2018$R0 <- recode(Dataframe2018$`Q281: Respondent - Occupational group`, `3` = 1)

Dataframe2018$R0 <- ifelse(Dataframe2018$R0 == 1, 1, 0)

table(Dataframe2018$R0)

table(Dataframe2018$`Q289: Religious denominations - major groups`)

Dataframe2018$R1 <- ifelse(Dataframe2018$`Q289: Religious denominations - major groups` == 2, 1, 0)

table(Dataframe2018$R1)

Dataframe2018$R2 <- Dataframe2018$Q171

table(Dataframe2018$`Q223: Which party would you vote for if there were a national election tomorrow`)

Dataframe2018$R3 <- ifelse(Dataframe2018$`Q223: Which party would you vote for if there were a national election tomorrow` == 76002, 1, 0)

table(Dataframe2018$R3)

Dataframe2018$R5 <- Dataframe2018$Q240

table(Dataframe2018$`Q275: Highest educational level: Respondent [ISCED 2011]`)

Dataframe2018$R6 <- ifelse(Dataframe2018$`Q275: Highest educational level: Respondent [ISCED 2011]` <= -1, NA, Dataframe2018$`Q275: Highest educational level: Respondent [ISCED 2011]`)

table(Dataframe2018$`G_TOWNSIZE: Settlement size_8 groups`)

Dataframe2018$R7 <- ifelse(Dataframe2018$`G_TOWNSIZE: Settlement size_8 groups` <= -1, NA, Dataframe2018$`G_TOWNSIZE: Settlement size_8 groups`)

table(Dataframe2018$`Q290: Ethnic group`)

Dataframe2018$R8 <- ifelse(Dataframe2018$`Q290: Ethnic group` == 76001, 1, 0)

table(Dataframe2018$R8)

table(Dataframe2018$`Q260: Sex`)

Dataframe2018$R9 <- Dataframe2018$`Q260: Sex`

table(Dataframe2018$`Q262: Age`)

Dataframe2018$R10 <- ifelse(Dataframe2018$`Q262: Age` <= -1, NA, Dataframe2018$`Q262: Age`)

table(Dataframe2018$`Q288: Scale of incomes`)

Dataframe2018$R11 <- ifelse(Dataframe2018$`Q288: Scale of incomes` <= -1, NA, Dataframe2018$`Q288: Scale of incomes`)

df2018reg <- Dataframe2018[, c("Q6","Q22","Q165","Q164","Q15","Q182","Q184","Q185","Q193","Q36","Q106","Q107","Q108","Q109",
                        "R1","R2","R3","R5","R6","R7","R8","R9","R10","R11")]


fa2018reg <- fa(df2018reg[,1:14], nfactors = 1, rotate = "varimax")

scores_df2018reg <- factor.scores(df2018reg[,1:14],fa2018reg)
scores_df2018reg <- scores_df2018reg$scores
df2018reg <- cbind(df2018reg,scores_df2018reg)

df2018reg$MR1 <- df2018reg$MR1 * (-1)

modelo2018reg <- lm(MR1~R1+R2+R3+R5+R6+R7+R8+R9+R10+R11, data=df2018reg)

df2018reg$dados_winsorized <- Winsorize(df2018reg$MR1, probs = c(0.05, 1), na.rm = TRUE)

#ACDD2019

table(Dataframe2019$relig)

Dataframe2019$relig[is.na(Dataframe2019$relig)] <- 0

Dataframe2019$R1 <- ifelse(Dataframe2019$relig == 5, 1, 0)

table(Dataframe2019$R1)

Dataframe2019$R2 <- Dataframe2019$V141

table(Dataframe2019$partsimp2)

Dataframe2019$partsimp2[is.na(Dataframe2019$partsimp2)] <- 0

Dataframe2019$R3 <- ifelse(Dataframe2019$partsimp2 == 30, 1, 0)

table(Dataframe2019$R3)

table(Dataframe2019$partsimpn2)

Dataframe2019$partsimpn2[is.na(Dataframe2019$partsimpn2)] <- 0

Dataframe2019$R4 <- ifelse(Dataframe2019$partsimpn2 == 30, 1, 0)

table(Dataframe2019$R4)

Dataframe2019$R5 <- Dataframe2019$ed

table(Dataframe2019$esc)

Dataframe2019$R6 <- Dataframe2019$esc

table(Dataframe2019$rcor)

Dataframe2019$R8 <- ifelse(Dataframe2019$rcor == 3, 1, 0)

table(Dataframe2019$R8)

table(Dataframe2019$sexo)

Dataframe2019$R9 <- Dataframe2019$sexo

table(Dataframe2019$idade)

Dataframe2019$R10 <- Dataframe2019$idade

table(Dataframe2019$fx.renda)

Dataframe2019$R11 <- Dataframe2019$fx.renda

df2019reg <- Dataframe2019[, c("V107","V102","V103","V105","V110","V115","V113","V114","V112",
                        "R1","R2","R3","R4","R5","R6","R8","R9","R10","R11")]

fa2019reg <- fa(df2019reg[,1:9], nfactors = 1, rotate = "varimax", cor = "mixed")

scores_df2019reg <- factor.scores(df2019reg[,1:9],fa2019reg)
scores_df2019reg <- scores_df2019reg$scores
df2019reg <- cbind(df2019reg,scores_df2019reg)

df2019reg$MR1 <- df2019reg$MR1 * (-1)

modelo2019reg <- lm(MR1~R1+R2+R3+R4+R5+R6+R8+R9+R10+R11, data=df2019reg)

df2019reg$dados_winsorized <- Winsorize(df2019reg$MR1, probs = c(0.05, 1), na.rm = TRUE)

#CLIVAGENS2023

table(Dataframe2023$P13)

Dataframe2023$R1 <- ifelse(Dataframe2023$P13 == 3, 1, 0)

table(Dataframe2023$P14)

Dataframe2023$R2 <- Dataframe2023$P14

table(Dataframe2023$P17)

Dataframe2023$P17[is.na(Dataframe2023$P17)] <- 0

Dataframe2023$R3 <- ifelse(Dataframe2023$P17 == 1, 1, 0)

table(Dataframe2023$R3)

table(Dataframe2023$P18)

Dataframe2023$R4 <- Dataframe2023$P18

table(Dataframe2023$P12)

Dataframe2023$R6 <- Dataframe2023$P12

table(Dataframe2023$P7)

Dataframe2023$R8 <- ifelse(Dataframe2023$P7 == 1, 1, 0)

table(Dataframe2023$R8)

table(Dataframe2023$P6)

Dataframe2023$R9 <- Dataframe2023$P6

table(Dataframe2023$P8)

Dataframe2023$R10 <- Dataframe2023$P8

table(Dataframe2023$P60)

Dataframe2023$R11 <- Dataframe2023$P60

df2023reg <- Dataframe2023[, c("P47","P40","P43",
                        "P35","P37","P36",
                        "R1","R2","R3","R4","R6","R8","R9","R10","R11")]

fa2023reg <- fa(df2023reg[,1:6], nfactors = 2, rotate = "varimax")

scores_df2023reg <- factor.scores(df2023reg[,1:6],fa2023reg)
scores_df2023reg <- scores_df2023reg$scores
df2023reg <- cbind(df2023reg,scores_df2023reg)

df2023reg$MR2 <- df2023reg$MR2 * (-1)

modelo2023reg <- lm(MR2~R1+R2+R3+R4+R6+R8+R9+R10+R11, data=df2023reg)

#Winsorizing

df2023reg$dados_winsorized <- Winsorize(df2023reg$MR2, probs = c(0.05, 1), na.rm = TRUE)

par(mfrow = c(3, 2))
hist(df1997reg$MR1, main = "Dados sem winzorização", xlab = "Fundamentalismo (1997)", ylab = "Frequência",  col = "lightblue", na.rm = TRUE)
hist(df1997reg$dados_winsorized, main = "Dados com winzorização", xlab = "Fundamentalismo (1997)", ylab = "", col = "lightgreen", na.rm = TRUE)
hist(df2006reg$MR1, main = "Dados sem winzorização", xlab = "Fundamentalismo (2006)", ylab = "Frequência",  col = "lightblue", na.rm = TRUE)
hist(df2006reg$dados_winsorized, main = "Dados com winzorização", xlab = "Fundamentalismo (2006)", ylab = "", col = "lightgreen", na.rm = TRUE)
hist(df2014reg$MR1, main = "Dados sem winzorização", xlab = "Fundamentalismo (2014)", ylab = "Frequência",  col = "lightblue", na.rm = TRUE)
hist(df2014reg$dados_winsorized, main = "Dados com winzorização", xlab = "Fundamentalismo (2014)", ylab = "", col = "lightgreen", na.rm = TRUE)
hist(df2018reg$MR1, main = "Dados sem winzorização", xlab = "Fundamentalismo (2018)", ylab = "Frequência",  col = "lightblue", na.rm = TRUE)
hist(df2018reg$dados_winsorized, main = "Dados com winzorização", xlab = "Fundamentalismo (2018)", ylab = "", col = "lightgreen", na.rm = TRUE)
hist(df2019reg$MR1, main = "Dados sem winzorização", xlab = "Fundamentalismo (2019)", ylab = "Frequência",  col = "lightblue", na.rm = TRUE)
hist(df2019reg$dados_winsorized, main = "Dados com winzorização", xlab = "Fundamentalismo (2019)", ylab = "", col = "lightgreen", na.rm = TRUE)
hist(df2023reg$MR1, main = "Dados sem winzorização", xlab = "Fundamentalismo (2023)", ylab = "Frequência",  col = "lightblue", na.rm = TRUE)
hist(df2023reg$dados_winsorized, main = "Dados com winzorização", xlab = "Fundamentalismo (2023)", ylab = "", col = "lightgreen", na.rm = TRUE)

#GRAFICO REGRESSAO

modelo1997reg <- lm(dados_winsorized~R1+R2+R3+R4+R5+R6+R7+R8+R9+R10+R11, data=df1997reg)
modelo2006reg <- lm(dados_winsorized~R1+R2+R3+R4+R5+R6+R7+R8+R9+R10+R11, data=df2006reg)
modelo2014reg <- lm(dados_winsorized~R1+R2+R3+R5+R6+R7+R8+R9+R10+R11, data=df2014reg)
modelo2018reg <- lm(dados_winsorized~R1+R2+R3+R5+R6+R7+R8+R9+R10+R11, data=df2018reg)
modelo2019reg <- lm(dados_winsorized~R1+R2+R3+R4+R5+R6+R8+R9+R10+R11, data=df2019reg)
modelo2023reg <- lm(dados_winsorized~R1+R2+R3+R4+R6+R8+R9+R10+R11, data=df2023reg)

#Assumptions regressões por ano

par(mfrow = c(2, 2))
plot(modelo1997reg)

par(mfrow = c(2, 2))
plot(modelo2006reg)

par(mfrow = c(2, 2))
plot(modelo2014reg)

par(mfrow = c(2, 2))
plot(modelo2018reg)

par(mfrow = c(2, 2))
plot(modelo2019reg)

par(mfrow = c(2, 2))
plot(modelo2023reg)

robust_se <- function(model) {
  coeftest(model, vcov = vcovHC(model, type = "HC0"))
}

mod19<-broom::tidy(robust_se(modelo2019reg))
mod19 %>% glimpse()

mod19 <- mod19 %>% mutate(ano = 2019)


mod97<-broom::tidy(robust_se(modelo1997reg))
mod97 %>% glimpse()

mod97 <- mod97 %>% mutate(ano = 1997)


mod06<-broom::tidy(robust_se(modelo2006reg))
mod06 %>% glimpse()

mod06 <- mod06 %>% mutate(ano = 2006)


mod14<-broom::tidy(robust_se(modelo2014reg))
mod14 %>% glimpse()

mod14 <- mod14 %>% mutate(ano = 2014)

mod18<-broom::tidy(robust_se(modelo2018reg))
mod18 %>% glimpse()

mod18 <- mod18 %>% mutate(ano = 2018)

mod23<-broom::tidy(robust_se(modelo2023reg))
mod23 %>% glimpse()

mod23 <- mod23 %>% mutate(ano = 2023)

modfinal <- rbind(mod97,mod06,mod14,mod18,mod19,mod23)
modfinal %>% glimpse()
table(modfinal$ano)


modfinal <- modfinal  %>%   mutate(upper = estimate + (1.96 * std.error),
                                   lower = estimate - (1.96 * std.error)) %>% 
  filter(term != '(Intercept)')  %>%   
  mutate(signif = ifelse(p.value <= 0.05, "Significante", "Não significante"))

novos_rotulos <- c(
  "R1" = "Evangélica",
  "R2" = "Religiosidade",
  "R3" = "Petismo",
  "R4" = "Anti-Petismo",
  "R5" = "Direita",
  "R6" = "Escolaridade",
  "R7" = "População",
  "R8" = "Branca",
  "R9" = "Mulher",
  "R10" = "Idade",
  "R11" = "Renda"
)

modfinal %>% ggplot(aes(x = factor(term, levels = names(novos_rotulos)), y = estimate, ymax = upper,
                        ymin = lower, color = signif)) +
  geom_pointrange() +
  coord_flip() +
  scale_color_manual(name = "", values = c("grey", "black")) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = "black") +
  theme_bw() +
  xlab("") +
  ylab("") +
  facet_wrap(~ano) +
  labs(subtitle = "") +
  theme(legend.position = "bottom",
        legend.margin = margin(t = -15),
        plot.subtitle = element_text(size = 10)) +
  scale_x_discrete(labels = novos_rotulos)

#grafico SEM WINSORIZING E ERRO PADRAO

modelo1997reg <- lm(MR1~R1+R2+R3+R4+R5+R6+R7+R8+R9+R10+R11, data=df1997reg)
modelo2006reg <- lm(MR1~R1+R2+R3+R4+R5+R6+R7+R8+R9+R10+R11, data=df2006reg)
modelo2014reg <- lm(MR1~R1+R2+R3+R5+R6+R7+R8+R9+R10+R11, data=df2014reg)
modelo2018reg <- lm(MR1~R1+R2+R3+R5+R6+R7+R8+R9+R10+R11, data=df2018reg)
modelo2019reg <- lm(MR1~R1+R2+R3+R4+R5+R6+R8+R9+R10+R11, data=df2019reg)
modelo2023reg <- lm(MR2~R1+R2+R3+R4+R6+R8+R9+R10+R11, data=df2023reg)

mod19<-broom::tidy(modelo2019reg)
mod19 %>% glimpse()

mod19 <- mod19 %>% mutate(ano = 2019)


mod97<-broom::tidy(modelo1997reg)
mod97 %>% glimpse()

mod97 <- mod97 %>% mutate(ano = 1997)


mod06<-broom::tidy(modelo2006reg)
mod06 %>% glimpse()

mod06 <- mod06 %>% mutate(ano = 2006)


mod14<-broom::tidy(modelo2014reg)
mod14 %>% glimpse()

mod14 <- mod14 %>% mutate(ano = 2014)

mod18<-broom::tidy(modelo2018reg)
mod18 %>% glimpse()

mod18 <- mod18 %>% mutate(ano = 2018)

mod23<-broom::tidy(modelo2023reg)
mod23 %>% glimpse()

mod23 <- mod23 %>% mutate(ano = 2023)

modfinal <- rbind(mod97,mod06,mod14,mod18,mod19,mod23)
modfinal %>% glimpse()
table(modfinal$ano)#para conferir


modfinal <- modfinal  %>%   mutate(upper = estimate + (1.96 * std.error),
                                   lower = estimate - (1.96 * std.error)) %>% 
  filter(term != '(Intercept)')  %>%   
  mutate(signif = ifelse(p.value <= 0.05, "Significante", "Não significante"))

novos_rotulos <- c(
  "R1" = "Evangélica",
  "R2" = "Religiosidade",
  "R3" = "Petismo",
  "R4" = "Anti-Petismo",
  "R5" = "Direita",
  "R6" = "Escolaridade",
  "R7" = "População",
  "R8" = "Branca",
  "R9" = "Mulher",
  "R10" = "Idade",
  "R11" = "Renda"
)

modfinal %>% ggplot(aes(x = factor(term, levels = names(novos_rotulos)), y = estimate, ymax = upper,
                        ymin = lower, color = signif)) +
  geom_pointrange() +
  coord_flip() +
  scale_color_manual(name = "", values = c("grey", "black")) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = "black") +
  theme_bw() +
  xlab("") +
  ylab("") +
  facet_wrap(~ano) +
  labs(subtitle = "") +
  theme(legend.position = "bottom",
        legend.margin = margin(t = -15),
        plot.subtitle = element_text(size = 10)) +
  scale_x_discrete(labels = novos_rotulos)

