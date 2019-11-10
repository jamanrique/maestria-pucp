library(tidyverse)
library(ISLR)
datos <- Default

head(datos)

# Se recodifican los niveles No, Yes a 0 y 1
datos <- datos %>%
  select(default, balance) %>%
  mutate(default = recode(default,
                          "No"  = 0,
                          "Yes" = 1))
head(datos)

# Modelo de regresión Lineal
modelo_lineal <- lm(default ~ balance, data = datos)

# Representación gráfica del modelo.
ggplot(data = datos, aes(x = balance, y = default)) +
  geom_point(aes(color = as.factor(default))) + 
  geom_smooth(method = "lm", color = "gray20") +
  theme_bw()  +
  labs(title = "Regresión lineal por mínimos cuadrados",
       y = "Probabilidad default") +
  theme(legend.position = "none")

predict(object = modelo_lineal, newdata = data.frame(balance = 100))

modelo_logistico <- glm(default ~ balance, data = datos, family = "binomial")

summary(modelo_logistico)

names(modelo_logistico)

vcov(modelo_logistico)

# Ajuste del modelo
pchisq(deviance(modelo_logistico),df.residual(modelo_logistico) ,lower=FALSE)

# "Estimación"" de y para los valores observados:
ypred <- predict(modelo_logistico, type = "response")
ypred[1:10]

# Representación gráfica del modelo.
ggplot(data = datos, aes(x = balance, y = default)) +
  geom_point(aes(color = as.factor(default)), shape = 1) + 
  stat_function(fun = function(x){predict(modelo_logistico,
                                          newdata = data.frame(balance = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística",
       y = "Probabilidad default") +
  theme(legend.position = "none")


