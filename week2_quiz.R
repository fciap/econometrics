# загружаем пакеты
library("memisc") # две и более регрессий в одной табличке
library("dplyr") # манипуляции с данными
library("psych") # описательные статистики
library("lmtest") # тестирование гипотез в линейных моделях
library("sjPlot") # графики
library("sgof")
library("ggplot2") # графики
library("foreign") # загрузка данных в разных форматах
library("car")
library("hexbin") # графики
library("rlms") # загрузка данных в формате rlms (spss)

pt(20/5, df=61)
round(2+qt(0.9, df=28)*0.3,2)

(0.75-0.15)/2

pchisq(9, df=10)
round(pt(4, df=2), 2)

data <- diamonds

glimpse(data)
describe(data)

vg_cut <- filter(data, cut=='Premium')

describe(vg_cut)

model <- lm(data=data, price ~ carat)
summary(model)

model_g <- glm(data=data, price ~ carat)
linearHypothesis(model_g, "carat=0")

model2 <- glm(data=data, price ~ carat + x + y + z)
summary(model2)

linearHypothesis(model2, "y=0")

comp_m_m2 <- mtable(model, model2)
comp_m_m2

model3 <- glm(data = data, price ~ carat + y + x)
confint(model3, level=0.9)
summary(model3)
 
round(126.00 + qnorm(0.95) *  25.76, 2)
