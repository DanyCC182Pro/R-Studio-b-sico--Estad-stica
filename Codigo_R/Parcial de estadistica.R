base2sep <- datos_ingenieria
#-------------------------------------------------------------------------------
# Calcular la media, mediana, moda
library(dplyr)

# Función para calcular la moda
moda <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}


# Calcular estadísticas descriptivas para Matemáticas
media_matematicas <- mean(base2sep$Matemáticas)
mediana_matematicas <- median(base2sep$Matemáticas)
moda_matematicas <- moda(base2sep$Matemáticas) 
min_matematicas <- min(base2sep$Matemáticas)
max_matematicas <- max(base2sep$Matemáticas)
desviacion_matematicas <- sd(base2sep$Matemáticas)
coef_var_matematicas <- desviacion_matematicas / media_matematicas

print(media_matematicas)
print(mediana_matematicas)
print(moda_matematicas)
print(min_matematicas)
print(max_matematicas)
print(desviacion_matematicas)
print(coef_var_matematicas)

# Histograma para Matemáticas
hist(base2sep$Matemáticas, 
     main = "Histograma de Calificaciones en Matemáticas", 
     xlab = "Calificación en Matemáticas", 
     col = "lightblue", 
     border = "black")

# Añadir líneas para la media, mediana y moda
abline(v = media_matematicas, col = "red", lwd = 2, lty = 2)  # Línea roja punteada para la media
abline(v = mediana_matematicas, col = "blue", lwd = 2, lty = 2)  # Línea azul punteada para la mediana
abline(v = moda_matematicas, col = "green", lwd = 2, lty = 2)  # Línea verde punteada para la moda

# Añadir una leyenda
legend("topright", legend = c(paste("Media:", round(media_matematicas, 2)), 
                              paste("Mediana:", round(mediana_matematicas, 2)),
                              paste("Moda:", round(moda_matematicas, 2))),
       col = c("red", "blue", "green"), lty = 2, lwd = 2)

# Cálculo de estadísticas descriptivas por Sexo
resumen_sexo <- base2sep %>%
  group_by(Sexo) %>%
  summarise(
    Media_Matematicas = mean(Matemáticas, na.rm = TRUE),
    Mediana_Matematicas = median(Matemáticas, na.rm = TRUE),
    Moda_Matematicas = moda(Matemáticas),
    Desv_Estandar_Matematicas = sd(Matemáticas, na.rm = TRUE),
    Minimo_Matematicas = min(Matemáticas, na.rm = TRUE),
    Maximo_Matematicas = max(Matemáticas, na.rm = TRUE),
    Coef_Variacion_Matematicas = sd(Matemáticas, na.rm = TRUE) / mean(Matemáticas, na.rm = TRUE),
  )

print(resumen_sexo)

# Cálculo de estadísticas descriptivas por Jornada
resumen_jornada <- base2sep %>%
  group_by(Jornada) %>%
  summarise(
    Media_Matematicas = mean(Matemáticas, na.rm = TRUE),
    Mediana_Matematicas = median(Matemáticas, na.rm = TRUE),
    Moda_Matematicas = moda(Matemáticas),
    Desv_Estandar_Matematicas = sd(Matemáticas, na.rm = TRUE),
    Minimo_Matematicas = min(Matemáticas, na.rm = TRUE),
    Maximo_Matematicas = max(Matemáticas, na.rm = TRUE),
    Coef_Variacion_Matematicas = sd(Matemáticas, na.rm = TRUE) / mean(Matemáticas, na.rm = TRUE),
  )

print(resumen_jornada)

resumen_estres <- base2sep %>%
  group_by(Nivel_Stress) %>%
  summarise(
    Media_Matematicas = mean(Matemáticas, na.rm = TRUE),
    Mediana_Matematicas = median(Matemáticas, na.rm = TRUE),
    Moda_Matematicas = moda(Matemáticas),
    Desv_Estandar_Matematicas = sd(Matemáticas, na.rm = TRUE),
    Minimo_Matematicas = min(Matemáticas, na.rm = TRUE),
    Maximo_Matematicas = max(Matemáticas, na.rm = TRUE),
    Coef_Variacion_Matematicas = sd(Matemáticas, na.rm = TRUE) / mean(Matemáticas, na.rm = TRUE),
  )

print(resumen_estres)
#-----------------------------------------------------------------------------
# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(purrr)
library(dplyr)

# Función para imprimir los valores atípicos por grupo
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
# Calcular los cuartiles de Matematicas
cuartiles_Mat <- quantile(base2sep$Matemáticas, na.rm = TRUE)
print(cuartiles_Mat)

# Crear un boxplot general de Matematicas
boxplot(base2sep$Matemáticas,
        main = "Boxplot de Matemáticas",
        ylab = "Nota",
        col = "lightblue")

# Añadir líneas que indiquen los cuartiles
abline(h = cuartiles_Mat, col = c("blue", "green", "red", "purple", "orange"), lty = 2)

# Añadir una leyenda para los cuartiles y la mediana
legend("topright", legend = c("1er Cuartil (Q1)", "Mediana (Q2)", "3er Cuartil (Q3)"),
       col = c("green", "red", "purple"), lty = 2, lwd = 2)



# Crear boxplot de Matemáticas segmentado por Sexo
boxplot(Matemáticas ~ Sexo, 
        data = base2sep, 
        main = "Boxplot de Matemáticas por Sexo", 
        ylab = "Nota", 
        col = "lightblue")


# Calcular estadísticas de Matemáticas por Sexo incluyendo valores atípicos
resumen_sexo_Ma <- base2sep %>%
  group_by(Sexo) %>%
  summarise(
    Minimo = min(Matemáticas, na.rm = TRUE),
    Q1 = quantile(Matemáticas, 0.25, na.rm = TRUE),
    Mediana = quantile(Matemáticas, 0.5, na.rm = TRUE),
    Q3 = quantile(Matemáticas, 0.75, na.rm = TRUE),
    Maximo_Boxplot = boxplot.stats(Matemáticas)$stats[5],  # Valor más alto del boxplot (no considerando atípicos)
    Atipicos = list(boxplot.stats(Matemáticas)$out)  # Valores atípicos
  )

# Mostrar resumen y valores atípicos por Sexo
print(resumen_sexo_Ma)
imprimir_atipicos(resumen_sexo_Ma, "Sexo")

# Crear boxplot de Matemáticas segmentado por Jornada
boxplot(Matemáticas ~ Jornada, 
        data = base2sep, 
        main = "Boxplot de Matemáticas por Jornada", 
        ylab = "Nota", 
        col = "lightblue")

# Calcular estadísticas de Matemáticas por Jornada incluyendo valores atípicos
resumen_jornada_Ma <- base2sep %>%
  group_by(Jornada) %>%
  summarise(
    Minimo = min(Matemáticas, na.rm = TRUE),
    Q1 = quantile(Matemáticas, 0.25, na.rm = TRUE),
    Mediana = quantile(Matemáticas, 0.5, na.rm = TRUE),
    Q3 = quantile(Matemáticas, 0.75, na.rm = TRUE),
    Maximo_Boxplot = boxplot.stats(Matemáticas)$stats[5],  # Valor más alto del boxplot (no considerando atípicos)
    Atipicos = list(boxplot.stats(Matemáticas)$out)  # Valores atípicos
  )

# Mostrar resumen y valores atípicos por Jornada
print(resumen_jornada_Ma)
imprimir_atipicos(resumen_jornada_Ma, "Jornada")

# Crear boxplot de Matemáticas segmentado por Nivel de Stress
boxplot(Matemáticas ~ Nivel_Stress, 
        data = base2sep, 
        main = "Boxplot de Matemáticas por Nivel de Stress", 
        ylab = "Nota", 
        col = "lightblue")

# Calcular estadísticas de Matemáticas por Nivel de Stress incluyendo valores atípicos
resumen_nivel_Ma <- base2sep %>%
  group_by(Nivel_Stress) %>%
  summarise(
    Minimo = min(Matemáticas, na.rm = TRUE),
    Q1 = quantile(Matemáticas, 0.25, na.rm = TRUE),
    Mediana = quantile(Matemáticas, 0.5, na.rm = TRUE),
    Q3 = quantile(Matemáticas, 0.75, na.rm = TRUE),
    Maximo_Boxplot = boxplot.stats(Matemáticas)$stats[5],  # Valor más alto del boxplot (no considerando atípicos)
    Atipicos = list(boxplot.stats(Matemáticas)$out)  # Valores atípicos
  )

# Mostrar resumen y valores atípicos por Nivel de Stress
print(resumen_nivel_Ma)
imprimir_atipicos(resumen_nivel_Ma, "Nivel_Stress")


#-----------------------------------------------------------------------------
# Calcular estadísticas descriptivas para Programación
media_programacion <- mean(base2sep$Programacion)
mediana_programacion <- median(base2sep$Programacion)
moda_programacion <- moda(base2sep$Programacion)
min_programacion <- min(base2sep$Programacion)
max_programacion <- max(base2sep$Programacion)
desviacion_programacion <- sd(base2sep$Programacion)
coef_var_programacion <- desviacion_programacion / media_programacion

print(media_programacion)
print(mediana_programacion)
print(moda_programacion)
print(min_programacion)
print(max_programacion)
print(desviacion_programacion)
print(coef_var_programacion)

# Histograma para programación
hist(base2sep$Programacion, 
     main = "Histograma de Calificaciones en Programación", 
     xlab = "Calificación en Programación", 
     col = "lightblue", 
     border = "black")

# Añadir líneas para la media, mediana y moda
abline(v = media_programacion, col = "red", lwd = 2, lty = 2)  # Línea roja punteada para la media
abline(v = mediana_programacion, col = "blue", lwd = 2, lty = 2)  # Línea azul punteada para la mediana
abline(v = moda_programacion, col = "green", lwd = 2, lty = 2)  # Línea verde punteada para la moda

# Añadir una leyenda
legend("topright", legend = c(paste("Media:", round(media_programacion, 2)), 
                              paste("Mediana:", round(mediana_programacion, 2)),
                              paste("Moda:", round(moda_programacion, 2))),
       col = c("red", "blue", "green"), lty = 2, lwd = 2)

# Cálculo de estadísticas descriptivas por Sexo
resumen_sexo_pro <- base2sep %>%
  group_by(Sexo) %>%
  summarise(
    Media_Programacion = mean(Programacion, na.rm = TRUE),
    Mediana_Programacion = median(Programacion, na.rm = TRUE),
    Moda_Programacion = moda(Programacion),
    Desv_Estandar_Programacion = sd(Programacion, na.rm = TRUE),
    Minimo_Programacion = min(Programacion, na.rm = TRUE),
    Maximo_Programacion = max(Programacion, na.rm = TRUE),
    Coef_Variacion_Programacion = sd(Programacion, na.rm = TRUE) / mean(Programacion, na.rm = TRUE),
  )
print(resumen_sexo_pro)

# Cálculo de estadísticas descriptivas por Jornada
resumen_jornadapro <- base2sep %>%
  group_by(Jornada) %>%
  summarise(
    Media_Programacion = mean(Programacion, na.rm = TRUE),
    Mediana_Programacion = median(Programacion, na.rm = TRUE),
    Moda_Programacion = moda(Programacion),
    Desv_Estandar_Programacion = sd(Programacion, na.rm = TRUE),
    Minimo_Programacion = min(Programacion, na.rm = TRUE),
    Maximo_Programacion = max(Programacion, na.rm = TRUE),
    Coef_Variacion_Programacion = sd(Programacion, na.rm = TRUE) / mean(Programacion, na.rm = TRUE),
  )

print(resumen_jornadapro)

resumen_estres_pro <- base2sep %>%
  group_by(Nivel_Stress) %>%
  summarise(
    Media_Programacion = mean(Programacion, na.rm = TRUE),
    Mediana_Programacion = median(Programacion, na.rm = TRUE),
    Moda_Programacion = moda(Programacion),
    Desv_Estandar_Programacion = sd(Programacion, na.rm = TRUE),
    Minimo_Programacion = min(Programacion, na.rm = TRUE),
    Maximo_Programacion = max(Programacion, na.rm = TRUE),
    Coef_Variacion_Programacion = sd(Programacion, na.rm = TRUE) / mean(Programacion, na.rm = TRUE),
  )

print(resumen_estres_pro)
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

# Calcular los cuartiles de Programacion
cuartiles_Prog <- quantile(base2sep$Programacion, na.rm = TRUE)
print(cuartiles_Prog)

# Crear un boxplot general de Programacion
boxplot(base2sep$Programacion,
        main = "Boxplot de Programacion",
        ylab = "Nota",
        col = "lightblue")

# Añadir líneas que indiquen los cuartiles
abline(h = cuartiles_Prog, col = c("blue", "green", "red", "purple", "orange"), lty = 2)

# Añadir una leyenda para los cuartiles y la mediana
legend("topright", legend = c("1er Cuartil (Q1)", "Mediana (Q2)", "3er Cuartil (Q3)"),
       col = c("green", "red", "purple"), lty = 2, lwd = 2)


#Programacion segmentado por Sexo
boxplot(Programacion ~ Sexo, 
        data = base2sep, 
        main = "Boxplot de Programación por Sexo", 
        ylab = "Nota", 
        col = "lightgreen")

# Calcular estadísticas de Programación por Sexo incluyendo valores atípicos
resumen_sexo_Pr <- base2sep %>%
  group_by(Sexo) %>%
  summarise(
    Minimo = min(Programacion, na.rm = TRUE),
    Q1 = quantile(Programacion, 0.25, na.rm = TRUE),
    Mediana = quantile(Programacion, 0.5, na.rm = TRUE),
    Q3 = quantile(Programacion, 0.75, na.rm = TRUE),
    Maximo_Boxplot = boxplot.stats(Programacion)$stats[5],  # Valor más alto del boxplot (no considerando atípicos)
    Atipicos = list(boxplot.stats(Programacion)$out)  # Valores atípicos
  )

# Mostrar resumen y valores atípicos por Sexo
print(resumen_sexo_Pr)
imprimir_atipicos(resumen_sexo_Pr, "Sexo")

# Crear boxplot de Programación segmentado por Jornada
boxplot(Programacion ~ Jornada, 
        data = base2sep, 
        main = "Boxplot de Programación por Jornada", 
        ylab = "Nota", 
        col = "lightgreen")

# Calcular estadísticas de Programación por Jornada incluyendo valores atípicos
resumen_jornada_Pr <- base2sep %>%
  group_by(Jornada) %>%
  summarise(
    Minimo = min(Programacion, na.rm = TRUE),
    Q1 = quantile(Programacion, 0.25, na.rm = TRUE),
    Mediana = quantile(Programacion, 0.5, na.rm = TRUE),
    Q3 = quantile(Programacion, 0.75, na.rm = TRUE),
    Maximo_Boxplot = boxplot.stats(Programacion)$stats[5],  # Valor más alto del boxplot (no considerando atípicos)
    Atipicos = list(boxplot.stats(Programacion)$out)  # Valores atípicos
  )

# Mostrar resumen y valores atípicos por Jornada
print(resumen_jornada_Pr)
imprimir_atipicos(resumen_jornada_Pr, "Jornada")

# Crear boxplot de Programación segmentado por Nivel de Stress
boxplot(Programacion ~ Nivel_Stress, 
        data = base2sep, 
        main = "Boxplot de Programación por Nivel de Stress", 
        ylab = "Nota", 
        col = "lightgreen")

# Calcular estadísticas de Programación por Nivel de Stress incluyendo valores atípicos
resumen_nivel_Pr <- base2sep %>%
  group_by(Nivel_Stress) %>%
  summarise(
    Minimo = min(Programacion, na.rm = TRUE),
    Q1 = quantile(Programacion, 0.25, na.rm = TRUE),
    Mediana = quantile(Programacion, 0.5, na.rm = TRUE),
    Q3 = quantile(Programacion, 0.75, na.rm = TRUE),
    Maximo_Boxplot = boxplot.stats(Programacion)$stats[5],  # Valor más alto del boxplot (no considerando atípicos)
    Atipicos = list(boxplot.stats(Programacion)$out)  # Valores atípicos
  )

# Mostrar resumen y valores atípicos por Nivel de Stress
print(resumen_nivel_Pr)
imprimir_atipicos(resumen_nivel_Pr, "Nivel_Stress")
#------------------------------------------------------------------------------

# Calcular estadísticas descriptivas para Trabajo en Equipo
media_trabajo <- mean(base2sep$Trabajo_en_Equipo)
mediana_trabajo <- median(base2sep$Trabajo_en_Equipo)
moda_trabajo <- moda(base2sep$Trabajo_en_Equipo) 
min_trabajo <- min(base2sep$Trabajo_en_Equipo)
max_trabajo <- max(base2sep$Trabajo_en_Equipo)
desviacion_trabajo <- sd(base2sep$Trabajo_en_Equipo)
coef_var_trabajo <- desviacion_trabajo / media_trabajo

print(media_trabajo)
print(mediana_trabajo)
print(moda_trabajo)
print(min_trabajo)
print(max_trabajo)
print(desviacion_trabajo)
print(coef_var_trabajo)

# Histograma para Trabajo en Equipo
hist(base2sep$Trabajo_en_Equipo, 
     main = "Histograma de Calificaciones en Trabajo en Equipo", 
     xlab = "Calificación en Trabajo en Equipo", 
     col = "lightblue", 
     border = "black")

# Añadir líneas para la media, mediana y moda
abline(v = media_trabajo, col = "red", lwd = 2, lty = 2)  # Línea roja punteada para la media
abline(v = mediana_trabajo, col = "blue", lwd = 2, lty = 2)  # Línea azul punteada para la mediana
abline(v = moda_trabajo, col = "green", lwd = 2, lty = 2)  # Línea verde punteada para la moda

# Añadir una leyenda
legend("topright", legend = c(paste("Media:", round(media_trabajo, 2)), 
                              paste("Mediana:", round(mediana_trabajo, 2)),
                              paste("Moda:", round(moda_trabajo, 2))),
       col = c("red", "blue", "green"), lty = 2, lwd = 2)

# Cálculo de estadísticas descriptivas por Sexo
resumen_sexo_tra <- base2sep %>%
  group_by(Sexo) %>%
  summarise(
    Media_Trabajo = mean(Trabajo_en_Equipo, na.rm = TRUE),
    Mediana_Trabajo = median(Trabajo_en_Equipo, na.rm = TRUE),
    Moda_Trabajo = moda(Trabajo_en_Equipo),
    Desv_Estandar_Trabajo = sd(Trabajo_en_Equipo, na.rm = TRUE),
    Minimo_Trabajo = min(Trabajo_en_Equipo, na.rm = TRUE),
    Maximo_Trabajo = max(Trabajo_en_Equipo, na.rm = TRUE),
    Coef_Variacion_Trabajo = sd(Trabajo_en_Equipo, na.rm = TRUE) / mean(Trabajo_en_Equipo, na.rm = TRUE)
  )
print(resumen_sexo_tra)

resumen_jornada_tra <- base2sep %>%
  group_by(Jornada) %>%
  summarise(
    Media_Trabajo = mean(Trabajo_en_Equipo, na.rm = TRUE),
    Mediana_Trabajo = median(Trabajo_en_Equipo, na.rm = TRUE),
    Moda_Trabajo = moda(Trabajo_en_Equipo),
    Desv_Estandar_Trabajo = sd(Trabajo_en_Equipo, na.rm = TRUE),
    Minimo_Trabajo = min(Trabajo_en_Equipo, na.rm = TRUE),
    Maximo_Trabajo = max(Trabajo_en_Equipo, na.rm = TRUE),
    Coef_Variacion_Trabajo = sd(Trabajo_en_Equipo, na.rm = TRUE) / mean(Trabajo_en_Equipo, na.rm = TRUE),
  )

print(resumen_jornada_tra)

resumen_estres_tra <- base2sep %>%
  group_by(Nivel_Stress) %>%
  summarise(
    Media_Trabajo = mean(Trabajo_en_Equipo, na.rm = TRUE),
    Mediana_Trabajo = median(Trabajo_en_Equipo, na.rm = TRUE),
    Moda_Trabajo = moda(Trabajo_en_Equipo),
    Desv_Estandar_Trabajo = sd(Trabajo_en_Equipo, na.rm = TRUE),
    Minimo_Trabajo = min(Trabajo_en_Equipo, na.rm = TRUE),
    Maximo_Trabajo = max(Trabajo_en_Equipo, na.rm = TRUE),
    Coef_Variacion_Trabajo = sd(Trabajo_en_Equipo, na.rm = TRUE) / mean(Trabajo_en_Equipo, na.rm = TRUE),
  )

print(resumen_estres_tra)

#------------------------------------------------------------------------------
# Calcular los cuartiles de Programacion
cuartiles_Tra <- quantile(base2sep$Trabajo_en_Equipo, na.rm = TRUE)
print(cuartiles_Tra)

# Crear un boxplot general de Programacion
boxplot(base2sep$Trabajo_en_Equipo,
        main = "Boxplot de Trabajo en Equipo",
        ylab = "Puntuacion",
        col = "lightblue")

# Añadir líneas que indiquen los cuartiles
abline(h = cuartiles_Tra, col = c("blue", "green", "red", "purple", "orange"), lty = 2)

# Añadir una leyenda para los cuartiles y la mediana
legend("topright", legend = c("1er Cuartil (Q1)", "Mediana (Q2)", "3er Cuartil (Q3)"),
       col = c("green", "red", "purple"), lty = 2, lwd = 2)


# Crear boxplot de Trabajo en Equipo segmentado por Sexo
boxplot(Trabajo_en_Equipo ~ Sexo, 
        data = base2sep, 
        main = "Boxplot de Trabajo en Equipo por Sexo", 
        ylab = "Puntuación", 
        col = "lightcoral")

# Calcular estadísticas de Trabajo en Equipo por Sexo incluyendo valores atípicos
resumen_sexo_Te <- base2sep %>%
  group_by(Sexo) %>%
  summarise(
    Minimo = min(Trabajo_en_Equipo, na.rm = TRUE),
    Q1 = quantile(Trabajo_en_Equipo, 0.25, na.rm = TRUE),
    Mediana = quantile(Trabajo_en_Equipo, 0.5, na.rm = TRUE),
    Q3 = quantile(Trabajo_en_Equipo, 0.75, na.rm = TRUE),
    Maximo_Boxplot = boxplot.stats(Trabajo_en_Equipo)$stats[5],  # Valor más alto del boxplot (no considerando atípicos)
    Atipicos = list(boxplot.stats(Trabajo_en_Equipo)$out)  # Valores atípicos
  )

# Mostrar resumen y valores atípicos por Sexo
print(resumen_sexo_Te)
imprimir_atipicos(resumen_sexo_Te, "Sexo")

# Crear boxplot de Trabajo en Equipo segmentado por Jornada
boxplot(Trabajo_en_Equipo ~ Jornada, 
        data = base2sep, 
        main = "Boxplot de Trabajo en Equipo por Jornada", 
        ylab = "Puntuación", 
        col = "lightcoral")

# Calcular estadísticas de Trabajo en Equipo por Jornada incluyendo valores atípicos
resumen_jornada_Te <- base2sep %>%
  group_by(Jornada) %>%
  summarise(
    Minimo = min(Trabajo_en_Equipo, na.rm = TRUE),
    Q1 = quantile(Trabajo_en_Equipo, 0.25, na.rm = TRUE),
    Mediana = quantile(Trabajo_en_Equipo, 0.5, na.rm = TRUE),
    Q3 = quantile(Trabajo_en_Equipo, 0.75, na.rm = TRUE),
    Maximo_Boxplot = boxplot.stats(Trabajo_en_Equipo)$stats[5],  # Valor más alto del boxplot (no considerando atípicos)
    Atipicos = list(boxplot.stats(Trabajo_en_Equipo)$out)  # Valores atípicos
  )

# Mostrar resumen y valores atípicos por Jornada
print(resumen_jornada_Te)
imprimir_atipicos(resumen_jornada_Te, "Jornada")

# Crear boxplot de Trabajo en Equipo segmentado por Nivel de Stress
boxplot(Trabajo_en_Equipo ~ Nivel_Stress, 
        data = base2sep, 
        main = "Boxplot de Trabajo en Equipo por Nivel de Stress", 
        ylab = "Puntuación", 
        col = "lightcoral")

# Calcular estadísticas de Trabajo en Equipo por Nivel de Stress incluyendo valores atípicos
resumen_nivel_Te <- base2sep %>%
  group_by(Nivel_Stress) %>%
  summarise(
    Minimo = min(Trabajo_en_Equipo, na.rm = TRUE),
    Q1 = quantile(Trabajo_en_Equipo, 0.25, na.rm = TRUE),
    Mediana = quantile(Trabajo_en_Equipo, 0.5, na.rm = TRUE),
    Q3 = quantile(Trabajo_en_Equipo, 0.75, na.rm = TRUE),
    Maximo_Boxplot = boxplot.stats(Trabajo_en_Equipo)$stats[5],  # Valor más alto del boxplot (no considerando atípicos)
    Atipicos = list(boxplot.stats(Trabajo_en_Equipo)$out)  # Valores atípicos
  )

# Mostrar resumen y valores atípicos por Nivel de Stress
print(resumen_nivel_Te)
imprimir_atipicos(resumen_nivel_Te, "Nivel_Stress")

#-----------------------------------------------------------------------------
library(readxl)
library(moments)
library(stats)
library(dplyr)
library(e1071)

# Calcular la asimetría y curtosis para Matemáticas, Programación y Trabajo en Equipo
asimetria_matematicas <- skewness(base2sep$Matemáticas, na.rm = TRUE)
curtosis_matematicas <- kurtosis(base2sep$Matemáticas, na.rm = TRUE)

asimetria_programacion <- skewness(base2sep$Programacion, na.rm = TRUE)
curtosis_programacion <- kurtosis(base2sep$Programacion, na.rm = TRUE)

asimetria_trabajo_equipo <- skewness(base2sep$Trabajo_en_Equipo, na.rm = TRUE)
curtosis_trabajo_equipo <- kurtosis(base2sep$Trabajo_en_Equipo, na.rm = TRUE)

cat("Matemáticas - Asimetría:", asimetria_matematicas, ", Curtosis:", curtosis_matematicas, "\n")
cat("Programación - Asimetría:", asimetria_programacion, ", Curtosis:", curtosis_programacion, "\n")
cat("Trabajo en Equipo - Asimetría:", asimetria_trabajo_equipo, ", Curtosis:", curtosis_trabajo_equipo, "\n")

# Calcular la curtosis por Sexo, Jornada y Nivel de Stress para Matemáticas, Programación y Trabajo en Equipo
curtosis_sexo_matematicas <- tapply(base2sep$Matemáticas, base2sep$Sexo, kurtosis, na.rm = TRUE)
curtosis_sexo_programacion <- tapply(base2sep$Programacion, base2sep$Sexo, kurtosis, na.rm = TRUE)
curtosis_sexo_trabajo_equipo <- tapply(base2sep$Trabajo_en_Equipo, base2sep$Sexo, kurtosis, na.rm = TRUE)

curtosis_jornada_matematicas <- tapply(base2sep$Matemáticas, base2sep$Jornada, kurtosis, na.rm = TRUE)
curtosis_jornada_programacion <- tapply(base2sep$Programacion, base2sep$Jornada, kurtosis, na.rm = TRUE)
curtosis_jornada_trabajo_equipo <- tapply(base2sep$Trabajo_en_Equipo, base2sep$Jornada, kurtosis, na.rm = TRUE)

curtosis_nivel_matematicas <- tapply(base2sep$Matemáticas, base2sep$Nivel_Stress, kurtosis, na.rm = TRUE)
curtosis_nivel_programacion <- tapply(base2sep$Programacion, base2sep$Nivel_Stress, kurtosis, na.rm = TRUE)
curtosis_nivel_trabajo_equipo <- tapply(base2sep$Trabajo_en_Equipo, base2sep$Nivel_Stress, kurtosis, na.rm = TRUE)

# Convertir los resultados a data.frame para una mejor presentación
curtosis_sexo <- data.frame(
  Sexo = names(curtosis_sexo_matematicas),
  Curtosis_Matematicas = curtosis_sexo_matematicas,
  Curtosis_Programacion = curtosis_sexo_programacion,
  Curtosis_Trabajo_Equipo = curtosis_sexo_trabajo_equipo
)

curtosis_jornada <- data.frame(
  Jornada = names(curtosis_jornada_matematicas),
  Curtosis_Matematicas = curtosis_jornada_matematicas,
  Curtosis_Programacion = curtosis_jornada_programacion,
  Curtosis_Trabajo_Equipo = curtosis_jornada_trabajo_equipo
)

curtosis_nivel <- data.frame(
  Nivel_Stress = names(curtosis_nivel_matematicas),
  Curtosis_Matematicas = curtosis_nivel_matematicas,
  Curtosis_Programacion = curtosis_nivel_programacion,
  Curtosis_Trabajo_Equipo = curtosis_nivel_trabajo_equipo
)

# Calcular la asimetría por Sexo, Jornada y Nivel de Stress para Matemáticas, Programación y Trabajo en Equipo
asimetria_sexo_matematicas <- tapply(base2sep$Matemáticas, base2sep$Sexo, skewness, na.rm = TRUE)
asimetria_sexo_programacion <- tapply(base2sep$Programacion, base2sep$Sexo, skewness, na.rm = TRUE)
asimetria_sexo_trabajo_equipo <- tapply(base2sep$Trabajo_en_Equipo, base2sep$Sexo, skewness, na.rm = TRUE)

asimetria_jornada_matematicas <- tapply(base2sep$Matemáticas, base2sep$Jornada, skewness, na.rm = TRUE)
asimetria_jornada_programacion <- tapply(base2sep$Programacion, base2sep$Jornada, skewness, na.rm = TRUE)
asimetria_jornada_trabajo_equipo <- tapply(base2sep$Trabajo_en_Equipo, base2sep$Jornada, skewness, na.rm = TRUE)

asimetria_nivel_matematicas <- tapply(base2sep$Matemáticas, base2sep$Nivel_Stress, skewness, na.rm = TRUE)
asimetria_nivel_programacion <- tapply(base2sep$Programacion, base2sep$Nivel_Stress, skewness, na.rm = TRUE)
asimetria_nivel_trabajo_equipo <- tapply(base2sep$Trabajo_en_Equipo, base2sep$Nivel_Stress, skewness, na.rm = TRUE)

# Convertir los resultados a data.frame para una mejor presentación
asimetria_sexo <- data.frame(
  Sexo = names(asimetria_sexo_matematicas),
  Asimetria_Matematicas = asimetria_sexo_matematicas,
  Asimetria_Programacion = asimetria_sexo_programacion,
  Asimetria_Trabajo_Equipo = asimetria_sexo_trabajo_equipo
)

asimetria_jornada <- data.frame(
  Jornada = names(asimetria_jornada_matematicas),
  Asimetria_Matematicas = asimetria_jornada_matematicas,
  Asimetria_Programacion = asimetria_jornada_programacion,
  Asimetria_Trabajo_Equipo = asimetria_jornada_trabajo_equipo
)

asimetria_nivel <- data.frame(
  Nivel_Stress = names(asimetria_nivel_matematicas),
  Asimetria_Matematicas = asimetria_nivel_matematicas,
  Asimetria_Programacion = asimetria_nivel_programacion,
  Asimetria_Trabajo_Equipo = asimetria_nivel_trabajo_equipo
)

# Mostrar resultados de curtosis
cat("Curtosis segmentada por Sexo:\n")
print(curtosis_sexo)
cat("\nCurtosis segmentada por Jornada:\n")
print(curtosis_jornada)
cat("\nCurtosis segmentada por Nivel de Stress:\n")
print(curtosis_nivel)

# Mostrar resultados de asimetría
cat("\nAsimetría segmentada por Sexo:\n")
print(asimetria_sexo)
cat("\nAsimetría segmentada por Jornada:\n")
print(asimetria_jornada)
cat("\nAsimetría segmentada por Nivel de Stress:\n")
print(asimetria_nivel)

# Crear una función para generar las gráficas
crear_grafica <- function(data, variable, titulo, curtosis, asimetria) {
  # Calcular la altura máxima estimada para la curva
  max_hist <- max(hist(data[[variable]], breaks = 10, plot = FALSE)$counts)
  max_curve <- max(dnorm(seq(min(data[[variable]], na.rm = TRUE), 
                             max(data[[variable]], na.rm = TRUE), 
                             length.out = 100), 
                         mean = mean(data[[variable]], na.rm = TRUE), 
                         sd = sd(data[[variable]], na.rm = TRUE)) * length(data[[variable]]) * 
                     diff(hist(data[[variable]], breaks = 10, plot = FALSE)$breaks)[1])
  
  # Ajustar ylim para que acomode tanto la curva como las barras del histograma
  ylim_max <- max(max_hist, max_curve)
  
  # Crear el histograma
  hist_data <- hist(data[[variable]], breaks = 10, col = rgb(0.1, 0.5, 0.8, 0.5), 
                    border = "white", main = titulo, xlab = variable, ylab = "Frecuencia", 
                    freq = TRUE, ylim = c(0, ylim_max * 1.1))
  
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

# Gráfica para Matemáticas
crear_grafica(base2sep, "Matemáticas", "Distribución de Matemáticas", curtosis_matematicas, asimetria_matematicas)

# Gráfica para Programación
crear_grafica(base2sep, "Programacion", "Distribución de Programación", curtosis_programacion, asimetria_programacion)

# Gráfica para Trabajo en Equipo
crear_grafica(base2sep, "Trabajo_en_Equipo", "Distribución de Trabajo en Equipo", curtosis_trabajo_equipo, asimetria_trabajo_equipo)



#------------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Correlaciones generales entre las variables
cor_matematicas_programacion <- cor(base2sep$Matemáticas, base2sep$Programacion, use = "complete.obs")
cor_matematicas_trabajo_equipo <- cor(base2sep$Matemáticas, base2sep$Trabajo_en_Equipo, use = "complete.obs")
cor_programacion_trabajo_equipo <- cor(base2sep$Programacion, base2sep$Trabajo_en_Equipo, use = "complete.obs")

# Modelos de regresión lineal
modelo_matematicas_programacion <- lm(Programacion ~ Matemáticas, data = base2sep)
modelo_matematicas_trabajo_equipo <- lm(Trabajo_en_Equipo ~ Matemáticas, data = base2sep)
modelo_programacion_trabajo_equipo <- lm(Trabajo_en_Equipo ~ Programacion, data = base2sep)

# Resumen de los modelos
summary(modelo_matematicas_programacion)
summary(modelo_matematicas_trabajo_equipo)
summary(modelo_programacion_trabajo_equipo)

library(dplyr)

# Función para calcular la correlación y el modelo lineal en un segmento específico
correlacion_modelo_segmentado <- function(data, var1, var2) {
  correlacion <- cor(data[[var1]], data[[var2]], use = "complete.obs")
  modelo <- lm(reformulate(var1, var2), data = data)
  return(list(correlacion = correlacion, modelo = modelo))
}

# Aplicar la función para cada segmento de Sexo
segmentos_sexo <- split(base2sep, base2sep$Sexo)
resultados_sexo <- lapply(segmentos_sexo, correlacion_modelo_segmentado, "Matemáticas", "Programacion")

# Aplicar la función para cada segmento de Jornada
segmentos_jornada <- split(base2sep, base2sep$Jornada)
resultados_jornada <- lapply(segmentos_jornada, correlacion_modelo_segmentado, "Matemáticas", "Programacion")

# Aplicar la función para cada segmento de Nivel de Stress
segmentos_nivel_stress <- split(base2sep, base2sep$Nivel_Stress)
resultados_nivel_stress <- lapply(segmentos_nivel_stress, correlacion_modelo_segmentado, "Matemáticas", "Programacion")

# Correlación en el segmento "Femenino"
correlacion_femenino <- resultados_sexo[["Femenino"]]$correlacion
print(paste("Correlación (femenino):", correlacion_femenino))
# Resumen del modelo en el segmento "Femenino"
modelo_Fe <- summary(resultados_sexo[["Femenino"]]$modelo)
print(modelo_Fe)
# Graficar Matemáticas vs Programación con la línea de regresión para el segmento Femenino
ggplot(segmentos_sexo[["Femenino"]], aes(x = Matemáticas, y = Programacion)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Regresión lineal: Matemáticas vs Programación (Femenino)",
       x = "Matemáticas", y = "Programación")



# Graficar Matemáticas vs Programación con la línea de regresión para el segmento Masculino

# Correlación en el segmento "Masculino"
correlacion_masculino <-resultados_sexo[["Masculino"]]$correlacion
print(paste("Correlación (masculino):", correlacion_masculino))
# Resumen del modelo en el segmento "Masculino"

modelo_Mas <- summary(resultados_sexo[["Masculino"]]$modelo)
print(modelo_Mas)
#Grafica Masculino
ggplot(segmentos_sexo[["Masculino"]], aes(x = Matemáticas, y = Programacion)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Regresión lineal: Matemáticas vs Programación (Masculino)",
       x = "Matemáticas", y = "Programación")


# Graficar Matemáticas vs Programación con la línea de regresión para el segmento noche

# Correlación en el segmento "noche"
correlacion_noche <- resultados_jornada[["noche"]]$correlacion
print(paste("Correlación (noche):", correlacion_noche))
# Resumen del modelo en el segmento "noche"

modelo_Noche <- summary(resultados_jornada[["noche"]]$modelo)
print(modelo_Noche)
#Grafica noche
ggplot(segmentos_jornada[["noche"]], aes(x = Matemáticas, y = Programacion)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Regresión lineal: Matemáticas vs Programación (noche)",
       x = "Matemáticas", y = "Programación")

# Graficar Matemáticas vs Programación con la línea de regresión para el segmento Dia

# Correlación en el segmento "Dia"
correlacion_Dia <-resultados_jornada[["Dia"]]$correlacion
print(paste("Correlación (Dia):", correlacion_Dia))
# Resumen del modelo en el segmento "Dia"

modelo_Dia <-summary(resultados_jornada[["Dia"]]$modelo)
print(modelo_Dia)

#Grafica Dia
ggplot(segmentos_jornada[["Dia"]], aes(x = Matemáticas, y = Programacion)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Regresión lineal: Matemáticas vs Programación (Dia)",
       x = "Matemáticas", y = "Programación")

# Graficar Matemáticas vs Programación con la línea de regresión para el segmento muy bajp

# Correlación en el segmento "Muy bajo"
correlacion_muybajo <-summary(resultados_nivel_stress[["Muy bajo"]]$correlacion)
print(paste("Correlación (Muy bajo):", correlacion_muybajo))

# Resumen del modelo en el segmento "Muy bajo"
modelo_muybajo <- summary(resultados_nivel_stress[["Muy bajo"]]$modelo)
print(modelo_muybajo)

ggplot(segmentos_nivel_stress[["Muy bajo"]], aes(x = Matemáticas, y = Programacion)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Regresión lineal: Matemáticas vs Programación (Muy bajo)",
       x = "Matemáticas", y = "Programación")

# 2. Segmento "Bajo"

# Correlación en el segmento "Bajo"
correlacion_bajo <- resultados_nivel_stress[["Bajo"]]$correlacion
print(paste("Correlación (Bajo):", correlacion_bajo))

# Resumen del modelo en el segmento "Bajo"
modelo_bajo <- summary(resultados_nivel_stress[["Bajo"]]$modelo)
print(modelo_bajo)

# Gráfico para "Bajo"
ggplot(segmentos_nivel_stress[["Bajo"]], aes(x = Matemáticas, y = Programacion)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Regresión lineal: Matemáticas vs Programación (Bajo)",
       x = "Matemáticas", y = "Programación")

# 3. Segmento "Medio"

# Correlación en el segmento "Medio"
correlacion_medio <- resultados_nivel_stress[["Medio"]]$correlacion
print(paste("Correlación (Medio):", correlacion_medio))

# Resumen del modelo en el segmento "Medio"
modelo_medio <- summary(resultados_nivel_stress[["Medio"]]$modelo)
print(modelo_medio)

# Gráfico para "Medio"
ggplot(segmentos_nivel_stress[["Medio"]], aes(x = Matemáticas, y = Programacion)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Regresión lineal: Matemáticas vs Programación (Medio)",
       x = "Matemáticas", y = "Programación")

# 3. Segmento "Alto"

# Correlación en el segmento "Alto"
correlacion_Alto <- resultados_nivel_stress[["Alto"]]$correlacion
print(paste("Correlación (Alto):", correlacion_Alto))

# Resumen del modelo en el segmento "Alto"
modelo_Alto <- summary(resultados_nivel_stress[["Alto"]]$modelo)
print(modelo_Alto)

# Gráfico para "Alto"
ggplot(segmentos_nivel_stress[["Alto"]], aes(x = Matemáticas, y = Programacion)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Regresión lineal: Matemáticas vs Programación (Alto)",
       x = "Matemáticas", y = "Programación")

# 4. Segmento "Muy alto"

# Correlación en el segmento "Muy alto"
correlacion_Muyalto <- resultados_nivel_stress[["Muy alto"]]$correlacion
print(paste("Correlación (Alto):", correlacion_Alto))

# Resumen del modelo en el segmento "Muy alto"
modelo_Muyalto <- summary(resultados_nivel_stress[["Muy alto"]]$modelo)
print(modelo_Muyalto)

# Gráfico para "Muy alto"
ggplot(segmentos_nivel_stress[["Muy alto"]], aes(x = Matemáticas, y = Programacion)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Regresión lineal: Matemáticas vs Programación (Muy alto)",
       x = "Matemáticas", y = "Programación")










