# Cargar las librerías necesarias
library(AER)
library(plm)
library(dplyr)
library(ggplot2)
library(forecast)

# Cargar la base de datos
data("Grunfeld", package = "AER")

# Filtrar los últimos 7 años
datos <- Grunfeld %>%
  filter(year >= max(year) - 6)

# Convertir a un formato de panel
panel_data <- pdata.frame(datos, index = c("firm", "year"))
print(head(panel_data))

# Ajustar el modelo de efectos fijos
mod_fijo <- plm(invest ~ value + capital, data = panel_data, model = "within")
cat("Resumen del modelo de efectos fijos (últimos 7 años):\n")
print(summary(mod_fijo))

# Ajustar el modelo de efectos aleatorios
mod_aleat <- plm(invest ~ value + capital, data = panel_data, model = "random")
cat("\nResumen del modelo de efectos aleatorios (últimos 7 años):\n")
print(summary(mod_aleat))

# Realizar el test de Hausman
hausman_test <- phtest(mod_fijo, mod_aleat)
cat("\nResultado del Test de Hausman (últimos 7 años):\n")
print(hausman_test)

# Interpretación del test de Hausman
if (hausman_test$p.value < 0.05) {
  cat("El modelo de efectos fijos es preferible según el test de Hausman.\n")
} else {
  cat("El modelo de efectos aleatorios es preferible según el test de Hausman.\n")
}

# Gráficas de correlación

# Relación entre valor y la inversión
ggplot(datos, aes(x = value, y = invest)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", col = "blue", se = FALSE) +
  labs(title = "Valor vs Inversión (Últimos 7 años)", x = "Valor", y = "Inversión") +
  theme_minimal()

# Relación entre capital y la inversión
ggplot(datos, aes(x = capital, y = invest)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", col = "green", se = FALSE) +
  labs(title = "Capital vs Inversión (Últimos 7 años)", x = "Capital", y = "Inversión") +
  theme_minimal()

# Crear una serie temporal agregada por año
serie_inv <- aggregate(invest ~ year, data = datos, sum)

# Convertir los datos en un objeto de serie temporal (ts)
serie_ts <- ts(serie_inv$invest, start = min(serie_inv$year), frequency = 1)

# Ajustar un modelo ARIMA con la función auto.arima
mod_arima <- auto.arima(serie_ts)
cat("\nResumen del modelo ARIMA:\n")
print(summary(mod_arima))

# Generar predicciones para los próximos 5 años
pred_arima <- forecast(mod_arima, h = 5)

# Crear un data frame para las predicciones
pred_df <- data.frame(
  year = c(serie_inv$year, (max(serie_inv$year) + 1):(max(serie_inv$year) + 5)),
  invest = c(serie_inv$invest, pred_arima$mean),
  lower = c(rep(NA, nrow(serie_inv)), pred_arima$lower[, 2]),
  upper = c(rep(NA, nrow(serie_inv)), pred_arima$upper[, 2])
)

# Graficar la serie original y las predicciones ARIMA con intervalos de confianza
ggplot(pred_df, aes(x = year, y = invest)) +
  geom_line(color = "blue") +  # Serie original y predicciones
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.3) +  # Intervalos de confianza
  labs(title = "Predicciones ARIMA con Intervalos de Confianza",
       x = "Año", y = "Inversión Agregada") +
  theme_minimal()
