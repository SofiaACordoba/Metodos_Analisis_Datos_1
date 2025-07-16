library(tidyverse)
library(car)

## Cargo datos #################################
juv <- read_rds('juveniles.rds')

# Exploro datos ####################################
juv


## Ajuste de modelo preliminar ######################
# Datos sin transformar
plot(peso_g ~ largo_mm, juv, pch=16)
m_1 <- lm(peso_g ~ largo_mm, data = juv)
abline(m_1)

# Gráfico de residuos vs. valores predichos:
plot(m_1, which = 1, add.smooth = FALSE, pch = 16)
# Supuestos: homocedasticidad? linealidad?

# QQplot del modelo preliminar:
plot(m_1, which = 2, pch = 16)


## Exploratorio: ¿transformación de Y? #######################

# Construyo "num_grupos" de intervalos a partir de la variable largo_mm (X)
num_grupos <- 8
juv$grupo <- cut(juv$largo_mm,
                 quantile(juv$largo_mm, 
                          seq(from = 0, to = 1, by = 1/num_grupos)),
                 include.lowest = TRUE)

# Visualizo los grupos
plot(peso_g ~ largo_mm, juv , pch=16,
     col = juv$grupo)

# Box-Cox para sugerir transformación
summary(powerTransform(peso_g ~ grupo, juv))
# sugiere log(Y), ya que estimación de lambda cercana a cero


## Diagrama de dispersión con log(peso_g) ~ largo_mm ######

plot(log(peso_g) ~ largo_mm, juv, pch=16)

# Ajusto modelo con log(Y)
m_2 <- lm(log(peso_g) ~ largo_mm, data = juv)
abline(m_2)

plot(m_2, which = 1, add.smooth = FALSE, pch = 16)
plot(m_2, which = 2, pch = 16)

# Evaluo transformaciones de X
invTranEstimate(juv$largo_mm, log(juv$peso_g))
invTranPlot(juv$largo_mm, log(juv$peso_g))
# sugiere log(X)


## Modelo final: log(peso_g) ~ log(largo_mm) ########

plot(log(peso_g) ~ log(largo_mm), juv, pch=16)

m_3 <- lm(log(peso_g) ~ log(largo_mm), juv)
abline(m_3)
summary(m_3)
coef(m_3)


## Bandas de predicción #########

grilla <- tibble(
  largo_mm = seq(from = min(juv$largo_mm), 
                 to = max(juv$largo_mm),
                 length = 200)  )

pred <- m_3 |>
  predict(newdata = grilla, interval = "prediction",
          level = 0.95) |>
  as_tibble()

datos_pred <- bind_cols(grilla, pred)

plot(log(peso_g) ~ log(largo_mm), juv, pch=16)
lines(fit ~ log(largo_mm), datos_pred)
lines(lwr ~ log(largo_mm), datos_pred, lty=3)
lines(upr ~ log(largo_mm), datos_pred, lty=3)

# Gráfico retransformado

plot(peso_g ~ largo_mm, juv, pch=16)
lines(exp(fit) ~ largo_mm, datos_pred)
lines(exp(lwr) ~ largo_mm, datos_pred, lty=3)
lines(exp(upr) ~ largo_mm, datos_pred, lty=3)


## ggplot versión ##############################

juv$predicho <- fitted(m_3)
juv$residuo <- residuals(m_3)
juv$restandar <- rstandard(m_3)

# Q-Q plot
ggplot(juv) + aes(sample = restandar) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q plot de los residuos",
       x = "Cuantiles teóricos",
       y = "Cuantiles de los residuos") +
  theme_light()

# Residuos vs. predichos
ggplot(juv) + 
  aes(predicho, residuo) +
  geom_hline(yintercept = 0, color = "grey30", linetype=3) +
  geom_point() +
  labs(x = "Predichos",
       y = "Residuos") +
  theme_bw()

# Dispersión log-log con bandas de predicción
ggplot(juv) +
  aes(log(largo_mm), log(peso_g)) +
  geom_ribbon(aes(x=log(largo_mm), 
                  y = NULL,
                  ymin = lwr,
                  ymax = upr), 
              data = datos_pred, alpha = 0.5, fill = "lightblue") +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  theme_bw() +
  labs(x = "ln ( Largo total (mm) )",
       y = "ln ( Peso (g) )")

# Gráfico retransformado

ggplot(datos_pred) +
  geom_ribbon(aes(x=largo_mm,
                  ymin= exp(lwr),
                  ymax= exp(upr)),
              alpha = 0.5, fill = "lightblue") +
  geom_line(aes(largo_mm, exp(fit)),
            color = "blue", linewidth=1) +
  geom_point(aes(largo_mm, peso_g), data = juv) +
  theme_bw() +
  labs(x = "Largo total (mm)",
       y = "Peso (g)")
