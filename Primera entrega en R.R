# Cargar las librerías necesarias
install.packages("dplyr")
library(dplyr)

# Asegúrate de que el data frame 'base_de_datos_1' esté correctamente cargado
str(base_de_datos_1)

# Renombrar el data frame para evitar conflictos
data29j <- base_de_datos_1

tabla_resumen <- data29j %>%
  group_by(`Estado Civil`, Genero) %>%
  summarise(Cantidad = n(), .groups = 'drop')

# Mostrar la tabla
print(tabla_resumen)

# Crear el primer gráfico de barras
barplot_heights <- tapply(tabla_resumen$Cantidad, list(tabla_resumen$`Estado Civil`, tabla_resumen$Genero), sum, na.rm = TRUE)
barplot_heights[is.na(barplot_heights)] <- 0 # Reemplazar NA con 0

# Crear gráfico de barras
barplot(barplot_heights, beside = TRUE, col = c("blue", "pink"), legend = TRUE,
        args.legend = list(x = "topright", legend = c("Hombre", "Mujer")),
        main = "Cantidad de Mujeres y Hombres por Estado Civil",
        xlab = "Estado Civil", ylab = "Cantidad")

# Crear segunda tabla de frecuencias
estado_civil_frecuencia <- table(data29j$`Estado Civil`)

# Crear gráfico de barras
barplot(estado_civil_frecuencia, col = "lightblue", main = "Distribución de Estados Civiles",
        xlab = "Estado Civil", ylab = "Frecuencia")

# Crear tercera tabla de frecuencias
genero_frecuencia <- table(data29j$Genero)

# Crear gráfico de barras
barplot(genero_frecuencia, col = c("blue", "pink"), main = "Distribución de Géneros",
        xlab = "Género", ylab = "Frecuencia")
