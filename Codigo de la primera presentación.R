# Daniel Esteban Cubillos Castiblanco 614221022 Presentación 05/08/2024
data29j <- base_de_datos_1


install.packages("dplyr")
library(dplyr)

# Mostrar los nombres de las columnas
colnames(data29j)


# Contar frecuencias de Estado Civil
estado_civil_freq <- table(data29j$`Estado Civil`)

# Contar frecuencias de Género
genero_freq <- table(data29j$Genero)

barplot_heights <- barplot(estado_civil_freq, 
                           main = "Frecuencia de Estado Civil", 
                           xlab = "Estado Civil", 
                           ylab = "Frecuencia", 
                           col = "blue", 
                           ylim = c(0, max(estado_civil_freq) + 5))

# Agregar las cantidades encima de las barras
text(x = barplot_heights, 
     y = estado_civil_freq, 
     label = estado_civil_freq, 
     pos = 3, 
     cex = 0.8, 
     col = "black")

# Crear el gráfico de pastel
pie(estado_civil_freq, 
    main = "Distribución de Estado Civil", 
    col = rainbow(length(estado_civil_freq)), 
    labels = paste0(names(estado_civil_freq), " - ", round(estado_civil_freq / sum(estado_civil_freq) * 100, 1), "%"))


# Datos de frecuencia
genero_freq <- c(Hombres = 119, Mujeres = 146)

# Calcular porcentajes
porcentajes <- round(genero_freq / sum(genero_freq) * 100, 1)
labels_barplot <- paste(genero_freq, "(", porcentajes, "%)", sep = "")

# Crear el gráfico de barras
barplot_heights <- barplot(genero_freq, 
                           main = "Distribución por Género", 
                           xlab = "Género", 
                           ylab = "Frecuencia", 
                           col = c("blue", "pink"), 
                           ylim = c(0, max(genero_freq) + 10))

# Agregar las cantidades y porcentajes encima de las barras
text(x = barplot_heights, 
     y = genero_freq, 
     label = labels_barplot, 
     pos = 3, 
     cex = 0.8, 
     col = "black")

# Crear el gráfico de pastel
pie(genero_freq, 
    main = "Distribución por Género", 
    col = c("blue", "pink"), 
    labels = paste0(names(genero_freq), " - ", porcentajes, "%"))

# Crear el histograma
hist_data <- hist(data29j$Hijos, 
                  main = "Distribución del Número de Hijos", 
                  xlab = "Número de Hijos", 
                  col = "lightblue", 
                  border = "black")

# Agregar las etiquetas de cantidad encima de las barras
text(hist_data$mids, hist_data$counts, labels = hist_data$counts, adj = c(0.5, -0.5), col = "black", cex = 0.8)

library(graphics)

# Crear la tabla de frecuencias
estado_hijos_table <- table(data29j$`Estado Civil`, data29j$Hijos)

# Crear el gráfico de barras apilado
barplot_heights <- barplot(estado_hijos_table, 
                           main = "Número de Hijos por Estado Civil", 
                           xlab = "Número de Hijos", 
                           ylab = "Frecuencia", 
                           col = rainbow(length(rownames(estado_hijos_table))), 
                           legend = rownames(estado_hijos_table), 
                           beside = TRUE,
                           ylim = c(0, max(estado_hijos_table) + 5))

# Agregar las etiquetas de cantidad encima de las barras
for(i in 1:ncol(estado_hijos_table)) {
  for(j in 1:nrow(estado_hijos_table)) {
    if (estado_hijos_table[j, i] > 0) {
      text(x = barplot_heights[j, i], 
           y = estado_hijos_table[j, i] + 0.5, 
           labels = estado_hijos_table[j, i], 
           cex = 0.8, 
           col = "black")
    }
  }
}



# Boxplot de Número de Hijos por Escolaridad
boxplot(Hijos ~ `Escolaridad (años)`, data = data29j, main = "Número de Hijos por Años de Escolaridad", xlab = "Años de Escolaridad", ylab = "Número de Hijos", col = "lightgreen")

# Calcular la cantidad de personas sin hijos
sin_hijos <- sum(data29j$Hijos == 0)
sin_hijos

# Calcular el promedio de hijos por familia
promedio_hijos <- mean(data29j$Hijos)
promedio_hijos

# Calcular la moda del número de hijos
moda_hijos <- as.numeric(names(sort(table(data29j$Hijos), decreasing = TRUE)[1]))
moda_hijos

# Calcular la desviación estándar del número de hijos
desviacion_hijos <- sd(data29j$Hijos)
desviacion_hijos

# Calcular las frecuencias de Estado Civil nuevamente 
estado_civil_freq <- table(data29j$`Estado Civil`)
estado_civil_freq

# Ordenar las frecuencias en orden descendente
estado_civil_freq <- sort(estado_civil_freq, decreasing = TRUE)
estado_civil_freq

# Calcular las frecuencias acumuladas y el porcentaje acumulado
frecuencia_acumulada <- cumsum(estado_civil_freq)
porcentaje_acumulado <- cumsum(estado_civil_freq) / sum(estado_civil_freq) * 100

# Convertir a data frame para facilitar la manipulación
pareto_data <- data.frame(Estado_Civil = names(estado_civil_freq), Frecuencia = as.vector(estado_civil_freq), Frecuencia_Acumulada = frecuencia_acumulada, Porcentaje_Acumulado = porcentaje_acumulado)
pareto_data

# Crear el diagrama de Pareto
barplot(pareto_data$Frecuencia, names.arg = pareto_data$Estado_Civil, col = "lightblue", main = "Diagrama de Pareto del Estado Civil", ylab = "Frecuencia")

# Agregar una línea de porcentaje acumulado
par(new = TRUE)
plot(pareto_data$Porcentaje_Acumulado, type = "o", col = "red", xaxt = "n", yaxt = "n", xlab = "", ylab = "", ylim = c(0, 100))
axis(side = 4, at = seq(0, 100, by = 10))
mtext("Porcentaje Acumulado", side = 4, line = 3)

# Instalar y cargar el paquete qcc si no está instalado
if(!require(qcc)) install.packages("qcc")
library(qcc)



# Crear la tabla de frecuencias
estado_civil_freq <- table(data29j$`Estado Civil`)

# Crear el diagrama de Pareto
# Crear el diagrama de Pareto con mejoras
pareto.chart(estado_civil_freq,
             main = "Diagrama de Pareto del Estado Civil",
             xlab = "Estado Civil",
             ylab = "Frecuencia",
             col = "lightblue",
             cex.names = 0.8, # Tamaño de las etiquetas del eje x
             las = 2) # Rotar etiquetas del eje x

# Tabla de frecuencia de años de escolaridad
escolaridad_freq <- table(data29j$`Escolaridad (años)`)

# Crear el gráfico de barras
barplot_heights <- barplot(escolaridad_freq, 
                           main = "Distribución de Años de Escolaridad", 
                           xlab = "Años de Escolaridad", 
                           ylab = "Frecuencia", 
                           col = "lightblue",
                           las = 2)

# Añadir etiquetas de frecuencia encima de las barras con un pequeño desplazamiento
text(x = barplot_heights, 
     y = escolaridad_freq + 1,  # Desplazamiento de +1 en la coordenada y
     label = escolaridad_freq, 
     pos = 3, 
     cex = 0.8, 
     col = "black")

# Crear boxplot de escolaridad por género
boxplot(data29j$`Escolaridad (años)` ~ data29j$Genero, 
        main = "Años de Escolaridad por Género", 
        xlab = "Género", 
        ylab = "Años de Escolaridad", 
        col = c("lightblue", "lightpink"))












