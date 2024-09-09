base26ago <- Musica_ejercicio_y_mucho_ma_s_
#------------------------------------------------------------------------------
# Calcular la media, mediana, moda del peso

library(dplyr)

# Función para calcular la moda
moda <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# PESO

media_peso <- mean(base26ago$Peso)
mediana_peso <- median(base26ago$Peso)
Moda_peso <- moda(base26ago$Peso)
min_peso <- min(base26ago$Peso)
max_peso <- max(base26ago$Peso)
desviacion_peso <- sd(base26ago$Peso)
Coeficiente_variacion_peso <- sd(base26ago$Peso) / mean(base26ago$Peso)

print(media_peso)
print(mediana_peso)
print(Moda_peso)
print(min_peso)
print(max_peso)
print(desviacion_peso)
print(Coeficiente_variacion_peso)


# Crear el histograma con la media y mediana resaltadas
hist(base26ago$Peso, 
     main = "Histograma del Peso", 
     xlab = "Peso (kg)", 
     col = "lightblue", 
     border = "black")

# Añadir líneas para la media y la mediana
abline(v = media_peso, col = "red", lwd = 2, lty = 2)  # Línea roja punteada para la media
abline(v = mediana_peso, col = "blue", lwd = 2, lty = 2)  # Línea azul punteada para la mediana
abline(v = Moda_peso, col = "green", lwd = 2, lty = 2)  # Línea verde punteada para la moda

# Añadir una leyenda
legend("topright", legend = c(paste("Media:", round(media_peso, 2)), 
                              paste("Mediana:", round(mediana_peso, 2)),
                              paste("Moda:", round(Moda_peso, 2))),
       col = c("red", "blue", "green"), lty = 2, lwd = 2)


resumen_gusto <- base26ago %>%
  group_by(GUSTO) %>%
  summarise(
    Media = mean(Peso, na.rm = TRUE),
    Mediana = median(Peso, na.rm = TRUE),
    Moda = moda(Peso),
    Desv_Estandar = sd(Peso, na.rm = TRUE),
    Minimo = min(Peso, na.rm = TRUE),
    Maximo = max(Peso, na.rm = TRUE),
    Coeficiente_Variacion = sd(Peso, na.rm = TRUE) / mean(Peso, na.rm = TRUE)
  )

print(resumen_gusto)


# Calcular estadísticas por NIVEL ACADEMICO
resumen_nivel <- base26ago %>%
  group_by(`NIVEL ACADEMICO`) %>%
  summarise(
    Media = mean(Peso, na.rm = TRUE),
    Mediana = median(Peso, na.rm = TRUE),
    Moda = moda(Peso),
    Desv_Estandar = sd(Peso, na.rm = TRUE),
    Minimo = min(Peso, na.rm = TRUE),
    Maximo = max(Peso, na.rm = TRUE),
    Coeficiente_Variacion = sd(Peso, na.rm = TRUE) / mean(Peso, na.rm = TRUE)
  )
print(resumen_nivel)

# Calcular estadísticas por VIVIENDA
resumen_vivienda <- base26ago %>%
  group_by(VIVIENDA) %>%
  summarise(
    Media = mean(Peso, na.rm = TRUE),
    Mediana = median(Peso, na.rm = TRUE),
    Moda = moda(Peso),
    Desv_Estandar = sd(Peso, na.rm = TRUE),
    Minimo = min(Peso, na.rm = TRUE),
    Maximo = max(Peso, na.rm = TRUE),
    Coeficiente_Variacion = sd(Peso, na.rm = TRUE) / mean(Peso, na.rm = TRUE)
  )
print(resumen_vivienda)
#-----------------------------------------------------------------------------
library(purrr)
library(dplyr)

imprimir_atipicos <- function(resumen, variable) {
  atipicos_text <- resumen %>%
    rowwise() %>%
    mutate(Atipicos_text = ifelse(length(unlist(Atipicos)) > 0,
                                  paste("Atípicos en", !!sym(variable), ":", toString(unique(unlist(Atipicos)))),
                                  paste("No hay atípicos en", !!sym(variable)))) %>%
    pull(Atipicos_text)
  
  for (text in atipicos_text) {
    cat(text, "\n")
  }
}


# Calcular los cuartiles de Peso
cuartiles_Peso <- quantile(base26ago$Peso, na.rm = TRUE)
print(cuartiles_Peso)

# Crear un boxplot general de peso
boxplot(base26ago$Peso,
        main = "Boxplot del PESO",
        ylab = "PESO (KG)",
        col = "lightblue")

# Añadir líneas que indiquen los cuartiles
abline(h = cuartiles_Peso, col = c("blue", "green", "red", "purple", "orange"), lty = 2)

# Añadir una leyenda para los cuartiles y la mediana
legend("topright", legend = c("1er Cuartil (Q1)", "Mediana (Q2)", "3er Cuartil (Q3)"),
       col = c("green", "red", "purple"), lty = 2, lwd = 2)

# Crear boxplot de peso segmentado por GUSTO
boxplot(Peso ~ GUSTO, 
        data = base26ago, 
        main = "Boxplot del Peso por GUSTO", 
        ylab = "PESO (KG)", 
        col = "lightblue")

# Calcular estadísticas de peso por GUSTO incluyendo valores atípicos
resumen_gusto_Pe <- base26ago %>%
  group_by(GUSTO) %>%
  summarise(
    Minimo = min(Peso, na.rm = TRUE),
    Q1 = quantile(Peso, 0.25, na.rm = TRUE),
    Mediana = quantile(Peso, 0.5, na.rm = TRUE),
    Q3 = quantile(Peso, 0.75, na.rm = TRUE),
    Maximo_Boxplot = boxplot.stats(Peso)$stats[5],  # Valor más alto del boxplot (no considerando atípicos)
    Atipicos = list(boxplot.stats(Peso)$out)  # Valores atípicos
  )

# Mostrar resumen y valores atípicos por GUSTO
print(resumen_gusto_Pe)
imprimir_atipicos(resumen_gusto_Pe, "GUSTO")

# Crear boxplot de peso segmentado por NIVEL ACADEMICO
boxplot(Peso ~ `NIVEL ACADEMICO`, 
        data = base26ago, 
        main = "Boxplot del peso por NIVEL ACADEMICO", 
        ylab = "peso (KG)", 
        col = "lightblue")

# Calcular estadísticas de peso por NIVEL ACADEMICO incluyendo valores atípicos
resumen_nivel_Pe <- base26ago %>%
  group_by(`NIVEL ACADEMICO`) %>%
  summarise(
    Minimo = min(Peso, na.rm = TRUE),
    Q1 = quantile(Peso, 0.25, na.rm = TRUE),
    Mediana = quantile(Peso, 0.5, na.rm = TRUE),
    Q3 = quantile(Peso, 0.75, na.rm = TRUE),
    Maximo_Boxplot = boxplot.stats(Peso)$stats[5],  # Valor más alto del boxplot (no considerando atípicos)
    Atipicos = list(boxplot.stats(Peso)$out)  # Valores atípicos
  )

# Mostrar resumen y valores atípicos por NIVEL ACADEMICO
print(resumen_nivel_Pe)
imprimir_atipicos(resumen_nivel_Pe, "NIVEL ACADEMICO")

# Crear boxplot de peso segmentado por VIVIENDA
boxplot(Peso ~ VIVIENDA, 
        data = base26ago, 
        main = "Boxplot del Peso por VIVIENDA", 
        ylab = "Peso (KG)", 
        col = "lightblue")

# Calcular estadísticas de peso por VIVIENDA incluyendo valores atípicos
resumen_vivienda_Pe <- base26ago %>%
  group_by(VIVIENDA) %>%
  summarise(
    Minimo = min(Peso, na.rm = TRUE),
    Q1 = quantile(Peso, 0.25, na.rm = TRUE),
    Mediana = quantile(Peso, 0.5, na.rm = TRUE),
    Q3 = quantile(Peso, 0.75, na.rm = TRUE),
    Maximo_Boxplot = boxplot.stats(Peso)$stats[5],  # Valor más alto del boxplot (no considerando atípicos)
    Atipicos = list(boxplot.stats(Peso)$out)  # Valores atípicos
  )

# Mostrar resumen y valores atípicos por VIVIENDA
print(resumen_vivienda_Pe)
imprimir_atipicos(resumen_vivienda_Pe, "VIVIENDA")


#------------------------------------------------------------------------------

# Talla

media_Talla <- mean(base26ago$Talla)
mediana_Talla <- median(base26ago$Talla)
Moda_Talla <- moda(base26ago$Talla)
min_Talla <- min(base26ago$Talla)
max_Talla <- max(base26ago$Talla)
desviacion_Talla <- sd(base26ago$Talla)
Coeficiente_variacion_Talla <- sd(base26ago$Talla) / mean(base26ago$Talla)

print(media_Talla)
print(mediana_Talla)
print(Moda_Talla)
print(min_Talla)
print(max_Talla)
print(desviacion_Talla)
print(Coeficiente_variacion_Talla)

# Crear el histograma con la media, mediana y moda resaltadas
hist(base26ago$Talla, 
     main = "Histograma de la talla", 
     xlab = "Talla (cm)", 
     col = "lightblue", 
     border = "black")

# Añadir líneas para la media y la mediana
abline(v = media_Talla, col = "red", lwd = 2, lty = 2)  # Línea roja punteada para la media
abline(v = mediana_Talla, col = "blue", lwd = 2, lty = 2)  # Línea azul punteada para la mediana
abline(v = Moda_Talla, col = "green", lwd = 2, lty = 2)  # Línea verde punteada para la moda

# Añadir una leyenda
legend("topright", legend = c(paste("Media:", round(media_Talla, 2)), 
                              paste("Mediana:", round(mediana_Talla, 2)),
                              paste("Moda:", round(Moda_Talla, 2))),
       col = c("red", "blue", "green"), lty = 2, lwd = 2)



# Calcular estadísticas por GUSTO
resumen_gusto <- base26ago %>%
  group_by(GUSTO) %>%
  summarise(
    Media = mean(Talla, na.rm = TRUE),
    Mediana = median(Talla, na.rm = TRUE),
    Moda = moda(Talla),
    Desv_Estandar = sd(Talla, na.rm = TRUE),
    Minimo = min(Talla, na.rm = TRUE),
    Maximo = max(Talla, na.rm = TRUE),
    Coeficiente_Variacion = sd(Talla, na.rm = TRUE) / mean(Talla, na.rm = TRUE)
  )

print(resumen_gusto)

# Calcular estadísticas por NIVEL ACADEMICO
resumen_nivel <- base26ago %>%
  group_by(`NIVEL ACADEMICO`) %>%
  summarise(
    Media = mean(Talla, na.rm = TRUE),
    Mediana = median(Talla, na.rm = TRUE),
    Moda = moda(Talla),
    Desv_Estandar = sd(Talla, na.rm = TRUE),
    Minimo = min(Talla, na.rm = TRUE),
    Maximo = max(Talla, na.rm = TRUE),
    Coeficiente_Variacion = sd(Talla, na.rm = TRUE) / mean(Talla, na.rm = TRUE)
  )

print(resumen_nivel)

# Calcular estadísticas por VIVIENDA
resumen_vivienda <- base26ago %>%
  group_by(VIVIENDA) %>%
  summarise(
    Media = mean(Talla, na.rm = TRUE),
    Mediana = median(Talla, na.rm = TRUE),
    Moda = moda(Talla),
    Desv_Estandar = sd(Talla, na.rm = TRUE),
    Minimo = min(Talla, na.rm = TRUE),
    Maximo = max(Talla, na.rm = TRUE),
    Coeficiente_Variacion = sd(Talla, na.rm = TRUE) / mean(Talla, na.rm = TRUE)
  )

print(resumen_vivienda)
#--------------------------------------------------------------------------
imprimir_atipicos <- function(resumen, variable) {
  atipicos_text <- resumen %>%
    rowwise() %>%
    mutate(Atipicos_text = ifelse(length(unlist(Atipicos)) > 0,
                                  paste("Atípicos en", !!sym(variable), ":", toString(unique(unlist(Atipicos)))),
                                  paste("No hay atípicos en", !!sym(variable)))) %>%
    pull(Atipicos_text)
  
  for (text in atipicos_text) {
    cat(text, "\n")
  }
}

# Calcular los cuartiles de tALLA
cuartiles_Talla <- quantile(base26ago$Talla)
print(cuartiles_Talla)

# Crear un boxplot general de Talla
boxplot(base26ago$Talla,
        main = "Boxplot de Talla",
        ylab = "Talla (cm)",
        col = "lightblue")

# Añadir líneas que indiquen los cuartiles
abline(h = cuartiles_Talla, col = c("blue","green", "red", "purple","orange"), lty = 2)

# Añadir una leyenda para los cuartiles y la mediana
legend("topright", legend = c("1er Cuartil (Q1)", "Mediana (Q2)", "3er Cuartil (Q3)"),
       col = c("green", "red", "purple"), lty = 2, lwd = 2)

# Crear boxplot de talla segmentado por GUSTO
boxplot(Talla ~ GUSTO, 
        data = base26ago, 
        main = "Boxplot de la talla por GUSTO", 
        ylab = "Talla (cm)", 
        col = "lightblue")



# Calcular estadísticas de Talla por GUSTO
resumen_gusto_TA <- base26ago %>%
  group_by(GUSTO) %>%
  summarise(
    Minimo = min(Talla, na.rm = TRUE),
    Q1 = quantile(Talla, 0.25, na.rm = TRUE),
    Mediana = quantile(Talla, 0.5, na.rm = TRUE),
    Q3 = quantile(Talla, 0.75, na.rm = TRUE),
    Maximo_Boxplot = boxplot.stats(Talla)$stats[5],  # Valor más alto del boxplot (no considerando atípicos)
    Atipicos = list(boxplot.stats(Talla)$out) # Identificación de atípicos
  )

print(resumen_gusto_TA)
imprimir_atipicos(resumen_gusto_TA, "GUSTO")

# Crear boxplot de TALLA segmentado por NIVEL ACADEMICO
boxplot(Talla ~ `NIVEL ACADEMICO`, 
        data = base26ago, 
        main = "Boxplot de la talla por NIVEL ACADEMICO", 
        ylab = "Talla (cm)", 
        col = "lightblue")

# Calcular estadísticas de Talla por NIVEL ACADEMICO
resumen_nivel_TA <- base26ago %>%
  group_by(`NIVEL ACADEMICO`) %>%
  summarise(
    Minimo = min(Talla, na.rm = TRUE),
    Q1 = quantile(Talla, 0.25, na.rm = TRUE),
    Mediana = quantile(Talla, 0.5, na.rm = TRUE),
    Q3 = quantile(Talla, 0.75, na.rm = TRUE),
    Maximo_Boxplot = boxplot.stats(Talla)$stats[5],
    Atipicos = list(boxplot.stats(Talla)$out)
  )

print(resumen_nivel_TA)
imprimir_atipicos(resumen_nivel_TA, "NIVEL ACADEMICO")

# Crear boxplot de TALLA segmentado por Vivienda
boxplot(Talla ~ VIVIENDA, 
        data = base26ago, 
        main = "Boxplot de la talla por Vivienda", 
        ylab = "Talla (cm)", 
        col = "lightblue")

# Calcular estadísticas de Talla por VIVIENDA
resumen_vivienda_ta <- base26ago %>%
  group_by(VIVIENDA) %>%
  summarise(
    Minimo = min(Talla, na.rm = TRUE),
    Q1 = quantile(Talla, 0.25, na.rm = TRUE),
    Mediana = quantile(Talla, 0.5, na.rm = TRUE),
    Q3 = quantile(Talla, 0.75, na.rm = TRUE),
    Maximo_Boxplot = boxplot.stats(Talla)$stats[5],
    Atipicos = list(boxplot.stats(Talla)$out)
  )

print(resumen_vivienda_ta)
imprimir_atipicos(resumen_vivienda_ta, "VIVIENDA")

#--------------------------------------------------------------------------

#SALARIO

# Calcular estadísticas generales de SALARIO
media_SALARIO <- mean(base26ago$SALARIO, na.rm = TRUE)
mediana_SALARIO <- median(base26ago$SALARIO, na.rm = TRUE)
Moda_SALARIO <- moda(base26ago$SALARIO)
min_SALARIO <- min(base26ago$SALARIO, na.rm = TRUE)
max_SALARIO <- max(base26ago$SALARIO, na.rm = TRUE)
desviacion_SALARIO <- sd(base26ago$SALARIO, na.rm = TRUE)
Coeficiente_variacion_SALARIO <- sd(base26ago$SALARIO, na.rm = TRUE) / mean(base26ago$SALARIO, na.rm = TRUE)

print(media_SALARIO)
print(mediana_SALARIO)
print(Moda_SALARIO)
print(min_SALARIO)
print(max_SALARIO)
print(desviacion_SALARIO)
print(Coeficiente_variacion_SALARIO)

# Crear el histograma con la media, mediana y moda resaltadas
hist(base26ago$SALARIO, 
     main = "Histograma del SALARIO", 
     xlab = "SALARIO (COP)", 
     col = "lightblue", 
     border = "black")

# Añadir líneas para la media, mediana y moda
abline(v = media_SALARIO, col = "red", lwd = 2, lty = 2)  # Línea roja punteada para la media
abline(v = mediana_SALARIO, col = "blue", lwd = 2, lty = 2)  # Línea azul punteada para la mediana
abline(v = Moda_SALARIO, col = "green", lwd = 2, lty = 2)  # Línea verde punteada para la moda

# Añadir una leyenda
legend("topright", legend = c(paste("Media:", round(media_SALARIO, 2)), 
                              paste("Mediana:", round(mediana_SALARIO, 2)),
                              paste("Moda:", round(Moda_SALARIO, 2))),
       col = c("red", "blue", "green"), lty = 2, lwd = 2)

# Calcular estadísticas de SALARIO por GUSTO
resumen_gusto <- base26ago %>%
  group_by(GUSTO) %>%
  summarise(
    Media = mean(SALARIO, na.rm = TRUE),
    Mediana = median(SALARIO, na.rm = TRUE),
    Moda = moda(SALARIO),
    Desv_Estandar = sd(SALARIO, na.rm = TRUE),
    Minimo = min(SALARIO, na.rm = TRUE),
    Maximo = max(SALARIO, na.rm = TRUE),
    Coeficiente_Variacion = sd(SALARIO, na.rm = TRUE) / mean(SALARIO, na.rm = TRUE)
  )

print(resumen_gusto)

# Calcular estadísticas de SALARIO por NIVEL ACADEMICO
resumen_nivel <- base26ago %>%
  group_by(`NIVEL ACADEMICO`) %>%
  summarise(
    Media = mean(SALARIO, na.rm = TRUE),
    Mediana = median(SALARIO, na.rm = TRUE),
    Moda = moda(SALARIO),
    Desv_Estandar = sd(SALARIO, na.rm = TRUE),
    Minimo = min(SALARIO, na.rm = TRUE),
    Maximo = max(SALARIO, na.rm = TRUE),
    Coeficiente_Variacion = sd(SALARIO, na.rm = TRUE) / mean(SALARIO, na.rm = TRUE)
  )

print(resumen_nivel)

# Calcular estadísticas de SALARIO por VIVIENDA
resumen_vivienda <- base26ago %>%
  group_by(VIVIENDA) %>%
  summarise(
    Media = mean(SALARIO, na.rm = TRUE),
    Mediana = median(SALARIO, na.rm = TRUE),
    Moda = moda(SALARIO),
    Desv_Estandar = sd(SALARIO, na.rm = TRUE),
    Minimo = min(SALARIO, na.rm = TRUE),
    Maximo = max(SALARIO, na.rm = TRUE),
    Coeficiente_Variacion = sd(SALARIO, na.rm = TRUE) / mean(SALARIO, na.rm = TRUE)
  )

print(resumen_vivienda)
#-----------------------------------------------------------------------------

imprimir_atipicos <- function(resumen, variable) {
  atipicos_text <- resumen %>%
    rowwise() %>%
    mutate(Atipicos_text = ifelse(length(unlist(Atipicos)) > 0,
                                  paste("Atípicos en", !!sym(variable), ":", toString(unique(unlist(Atipicos)))),
                                  paste("No hay atípicos en", !!sym(variable)))) %>%
    pull(Atipicos_text)
  
  for (text in atipicos_text) {
    cat(text, "\n")
  }
}


# Calcular los cuartiles de SALARIO
cuartiles_SALARIO <- quantile(base26ago$SALARIO)
print(cuartiles_SALARIO)

# Crear un boxplot general de SALARIO
boxplot(base26ago$SALARIO,
        main = "Boxplot del SALARIO",
        ylab = "SALARIO (COP)",
        col = "lightblue")

# Añadir líneas que indiquen los cuartiles
abline(h = cuartiles_SALARIO, col = c("blue","green", "red", "purple","orange"), lty = 2)

# Añadir una leyenda para los cuartiles y la mediana
legend("topright", legend = c("1er Cuartil (Q1)", "Mediana (Q2)", "3er Cuartil (Q3)"),
       col = c("green", "red", "purple"), lty = 2, lwd = 2)

# Crear boxplot de SALARIO segmentado por GUSTO
boxplot(SALARIO ~ GUSTO, 
        data = base26ago, 
        main = "Boxplot del SALARIO por GUSTO", 
        ylab = "SALARIO (COP)", 
        col = "lightblue")


# Calcular estadísticas de SALARIO por GUSTO
resumen_gusto <- base26ago %>%
  group_by(GUSTO) %>%
  summarise(
    Minimo = min(SALARIO, na.rm = TRUE),
    Q1 = quantile(SALARIO, 0.25, na.rm = TRUE),
    Mediana = quantile(SALARIO, 0.5, na.rm = TRUE),
    Q3 = quantile(SALARIO, 0.75, na.rm = TRUE),
    Maximo_Boxplot = boxplot.stats(SALARIO)$stats[5],
    Atipicos = list(boxplot.stats(SALARIO)$out)
  )

print(resumen_gusto)
imprimir_atipicos(resumen_gusto, "GUSTO")

# Crear boxplot de SALARIO segmentado por NIVEL ACADEMICO
boxplot(SALARIO ~ `NIVEL ACADEMICO`, 
        data = base26ago, 
        main = "Boxplot del SALARIO por NIVEL ACADEMICO", 
        ylab = "SALARIO (COP)", 
        col = "lightblue")

# Calcular estadísticas de SALARIO por NIVEL ACADEMICO
resumen_nivel <- base26ago %>%
  group_by(`NIVEL ACADEMICO`) %>%
  summarise(
    Minimo = min(SALARIO, na.rm = TRUE),
    Q1 = quantile(SALARIO, 0.25, na.rm = TRUE),
    Mediana = quantile(SALARIO, 0.5, na.rm = TRUE),
    Q3 = quantile(SALARIO, 0.75, na.rm = TRUE),
    Maximo_Boxplot = boxplot.stats(SALARIO)$stats[5],
    Atipicos = list(boxplot.stats(SALARIO)$out)
  )

print(resumen_nivel)
imprimir_atipicos(resumen_nivel, "NIVEL ACADEMICO")

# Crear boxplot de SALARIO segmentado por VIVIENDA
boxplot(SALARIO ~ VIVIENDA, 
        data = base26ago, 
        main = "Boxplot del SALARIO por VIVIENDA", 
        ylab = "SALARIO (COP)", 
        col = "lightblue")



# Calcular estadísticas de SALARIO por VIVIENDA
resumen_vivienda <- base26ago %>%
  group_by(VIVIENDA) %>%
  summarise(
    Minimo = min(SALARIO, na.rm = TRUE),
    Q1 = quantile(SALARIO, 0.25, na.rm = TRUE),
    Mediana = quantile(SALARIO, 0.5, na.rm = TRUE),
    Q3 = quantile(SALARIO, 0.75, na.rm = TRUE),
    Maximo_Boxplot = boxplot.stats(SALARIO)$stats[5],
    Atipicos = list(boxplot.stats(SALARIO)$out)
  )

print(resumen_vivienda)
imprimir_atipicos(resumen_vivienda, "VIVIENDA")


#-----------------------------------------------------------------------------

library(readxl)
library(moments)
library(stats)
library(dplyr)

# Cálculo de la asimetría y curtosis
asimetria_peso <- skewness(base26ago$Peso, na.rm = TRUE)
curtosis_peso <- kurtosis(base26ago$Peso, na.rm = TRUE)

asimetria_talla <- skewness(base26ago$Talla, na.rm = TRUE)
curtosis_talla <- kurtosis(base26ago$Talla, na.rm = TRUE)

asimetria_salario <- skewness(base26ago$SALARIO, na.rm = TRUE)
curtosis_salario <- kurtosis(base26ago$SALARIO, na.rm = TRUE)


cat("Peso - Asimetría:", asimetria_peso, ", Curtosis:", curtosis_peso, "\n")
cat("Talla - Asimetría:", asimetria_talla, ", Curtosis:", curtosis_talla, "\n")
cat("Salario - Asimetría:", asimetria_salario, ", Curtosis:", curtosis_salario, "\n")

# Calcular la curtosis por GUSTO, NIVEL ACADEMICO, y VIVIENDA para Peso, Talla, y Salario
curtosis_gusto_peso <- tapply(base26ago$Peso, base26ago$GUSTO, kurtosis, na.rm = TRUE)
curtosis_gusto_talla <- tapply(base26ago$Talla, base26ago$GUSTO, kurtosis, na.rm = TRUE)
curtosis_gusto_salario <- tapply(base26ago$SALARIO, base26ago$GUSTO, kurtosis, na.rm = TRUE)

curtosis_nivel_peso <- tapply(base26ago$Peso, base26ago$`NIVEL ACADEMICO`, kurtosis, na.rm = TRUE)
curtosis_nivel_talla <- tapply(base26ago$Talla, base26ago$`NIVEL ACADEMICO`, kurtosis, na.rm = TRUE)
curtosis_nivel_salario <- tapply(base26ago$SALARIO, base26ago$`NIVEL ACADEMICO`, kurtosis, na.rm = TRUE)

curtosis_vivienda_peso <- tapply(base26ago$Peso, base26ago$VIVIENDA, kurtosis, na.rm = TRUE)
curtosis_vivienda_talla <- tapply(base26ago$Talla, base26ago$VIVIENDA, kurtosis, na.rm = TRUE)
curtosis_vivienda_salario <- tapply(base26ago$SALARIO, base26ago$VIVIENDA, kurtosis, na.rm = TRUE)

# Convertir los resultados a data.frame para una mejor presentación
curtosis_gusto <- data.frame(
  GUSTO = names(curtosis_gusto_peso),
  Curtosis_Peso = curtosis_gusto_peso,
  Curtosis_Talla = curtosis_gusto_talla,
  Curtosis_Salario = curtosis_gusto_salario
)

curtosis_nivel <- data.frame(
  NIVEL_ACADEMICO = names(curtosis_nivel_peso),
  Curtosis_Peso = curtosis_nivel_peso,
  Curtosis_Talla = curtosis_nivel_talla,
  Curtosis_Salario = curtosis_nivel_salario
)

curtosis_vivienda <- data.frame(
  VIVIENDA = names(curtosis_vivienda_peso),
  Curtosis_Peso = curtosis_vivienda_peso,
  Curtosis_Talla = curtosis_vivienda_talla,
  Curtosis_Salario = curtosis_vivienda_salario
)

# Calcular la asimetría por GUSTO, NIVEL ACADEMICO, y VIVIENDA para Peso, Talla, y Salario
asimetria_gusto_peso <- tapply(base26ago$Peso, base26ago$GUSTO, skewness, na.rm = TRUE)
asimetria_gusto_talla <- tapply(base26ago$Talla, base26ago$GUSTO, skewness, na.rm = TRUE)
asimetria_gusto_salario <- tapply(base26ago$SALARIO, base26ago$GUSTO, skewness, na.rm = TRUE)

asimetria_nivel_peso <- tapply(base26ago$Peso, base26ago$`NIVEL ACADEMICO`, skewness, na.rm = TRUE)
asimetria_nivel_talla <- tapply(base26ago$Talla, base26ago$`NIVEL ACADEMICO`, skewness, na.rm = TRUE)
asimetria_nivel_salario <- tapply(base26ago$SALARIO, base26ago$`NIVEL ACADEMICO`, skewness, na.rm = TRUE)

asimetria_vivienda_peso <- tapply(base26ago$Peso, base26ago$VIVIENDA, skewness, na.rm = TRUE)
asimetria_vivienda_talla <- tapply(base26ago$Talla, base26ago$VIVIENDA, skewness, na.rm = TRUE)
asimetria_vivienda_salario <- tapply(base26ago$SALARIO, base26ago$VIVIENDA, skewness, na.rm = TRUE)

# Convertir los resultados a data.frame para una mejor presentación
asimetria_gusto <- data.frame(
  GUSTO = names(asimetria_gusto_peso),
  Asimetria_Peso = asimetria_gusto_peso,
  Asimetria_Talla = asimetria_gusto_talla,
  Asimetria_Salario = asimetria_gusto_salario
)

asimetria_nivel <- data.frame(
  NIVEL_ACADEMICO = names(asimetria_nivel_peso),
  Asimetria_Peso = asimetria_nivel_peso,
  Asimetria_Talla = asimetria_nivel_talla,
  Asimetria_Salario = asimetria_nivel_salario
)

asimetria_vivienda <- data.frame(
  VIVIENDA = names(asimetria_vivienda_peso),
  Asimetria_Peso = asimetria_vivienda_peso,
  Asimetria_Talla = asimetria_vivienda_talla,
  Asimetria_Salario = asimetria_vivienda_salario
)

# Mostrar resultados de curtosis
cat("Curtosis segmentada por GUSTO:\n")
print(curtosis_gusto)
cat("\nCurtosis segmentada por NIVEL ACADEMICO:\n")
print(curtosis_nivel)
cat("\nCurtosis segmentada por VIVIENDA:\n")
print(curtosis_vivienda)

# Mostrar resultados de asimetría
cat("\nAsimetría segmentada por GUSTO:\n")
print(asimetria_gusto)
cat("\nAsimetría segmentada por NIVEL ACADEMICO:\n")
print(asimetria_nivel)
cat("\nAsimetría segmentada por VIVIENDA:\n")
print(asimetria_vivienda)

# Cargar librerías necesarias
library(e1071)

# Crear el histograma con opciones mejoradas
hist_data <- hist(base26ago$Peso, breaks = 10, col = rgb(0.1, 0.5, 0.8, 0.5), border = "white", 
                  main = "Distribución del Peso", xlab = "Peso (kg)", ylab = "Frecuencia", 
                  freq = TRUE)

# Añadir texto de curtosis y asimetría en la esquina superior derecha
mtext(paste("Curtosis:", round(curtosis_peso, 2)), side = 3, adj = 1, line = -1.5, cex = 0.9)
mtext(paste("Asimetría:", round(asimetria_peso, 2)), side = 3, adj = 1, line = -2.5, cex = 0.9)

# Añadir una curva normal para comparación
curve(dnorm(x, mean=mean(base26ago$Peso, na.rm=TRUE), sd=sd(base26ago$Peso, na.rm=TRUE)) * length(base26ago$Peso) * diff(hist_data$breaks)[1], 
      col="darkblue", lwd=2, add=TRUE)

# Añadir líneas verticales que indiquen la curtosis y la asimetría
abline(v=mean(base26ago$Peso, na.rm=TRUE), col="red", lwd=2)

library(e1071)

# Calcular métricas de cada variable
curtosis_peso <- kurtosis(base26ago$Peso, na.rm = TRUE)
asimetria_peso <- skewness(base26ago$Peso, na.rm = TRUE)

curtosis_talla <- kurtosis(base26ago$Talla, na.rm = TRUE)
asimetria_talla <- skewness(base26ago$Talla, na.rm = TRUE)

curtosis_salario <- kurtosis(base26ago$SALARIO, na.rm = TRUE)
asimetria_salario <- skewness(base26ago$SALARIO, na.rm = TRUE)

# Crear una función para generar las gráficas
crear_grafica <- function(data, variable, titulo, curtosis, asimetria) {
  # Crear el histograma
  hist_data <- hist(data[[variable]], breaks = 10, col = rgb(0.1, 0.5, 0.8, 0.5), border = "white", 
                    main = titulo, xlab = variable, ylab = "Frecuencia", freq = TRUE)
  
  # Añadir texto de curtosis y asimetría en la esquina superior derecha
  mtext(paste("Curtosis:", round(curtosis, 2)), side = 3, adj = 1, line = -1.5, cex = 0.9)
  mtext(paste("Asimetría:", round(asimetria, 2)), side = 3, adj = 1, line = -2.5, cex = 0.9)
  
  # Añadir una curva normal para comparación
  curve(dnorm(x, mean=mean(data[[variable]], na.rm=TRUE), sd=sd(data[[variable]], na.rm=TRUE)) * length(data[[variable]]) * diff(hist_data$breaks)[1], 
        col="darkblue", lwd=2, add=TRUE)
  
  # Añadir línea vertical que indique la media
  abline(v=mean(data[[variable]], na.rm=TRUE), col="red", lwd=2)
}

# Generar gráficas para cada variable usando las métricas pre-calculadas
par(mfrow=c(3, 1))  # Dividir la ventana gráfica en 3 filas

# Gráfica para Peso
crear_grafica(base26ago, "Peso", "Distribución del Peso", curtosis_peso, asimetria_peso)

# Gráfica para Talla
crear_grafica(base26ago, "Talla", "Distribución de la Talla", curtosis_talla, asimetria_talla)

# Gráfica para Salario
crear_grafica(base26ago, "SALARIO", "Distribución del Salario", curtosis_salario, asimetria_salario)














