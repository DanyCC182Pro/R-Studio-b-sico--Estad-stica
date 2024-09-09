# Sobre que es Curtosis , Asimetría , Desviación , Varianza, CV. (segmentado)
# Los box plot Faltantes. 

library(dplyr)

base5ago <- base_de_datos_1


# ----------------------------------------------------------------------------
# Primero definimos que es la Desviación:
# La desviación estándar es una medida que cuantifica la cantidad de
# variabilidad o dispersión de un conjunto de datos. En términos simples, 
# nos dice qué tan dispersos están los datos alrededor de la media. 
# Una desviación estándar alta indica que los datos están más dispersos,
# mientras que una desviación estándar baja indica que los datos 
# están más concentrados alrededor de la media. 

# Fórmula de la desviación estándar en la Muestra:
# s = sqrt(1/(N-1) * sum((x_i - mean(x))^2))
#
# donde:
# s = desviación estándar
# N = número de observaciones
# x_i = cada valor individual en el conjunto de datos
# mean(x) = media de los valores

# Fórmula de la desviación estándar en la Población:
# R = sqrt(1/(N) * sum((x_i - mean(x))^2))
#
# donde:
# R = desviación estándar
# N = número de observaciones
# x_i = cada valor individual en el conjunto de datos
# mean(x) = media de los valores

# Desviación estándar general de Hijos y Escolaridad (años)
desviacion_general_hijos <- sd(base5ago$Hijos, na.rm = TRUE)
desviacion_general_escolaridad <- sd(base5ago$`Escolaridad (años)`, na.rm = TRUE)

# Imprimir resultados generales
print(paste("Desviación Estándar General de Hijos:", desviacion_general_hijos))
print(paste("Desviación Estándar General de Escolaridad (años):", desviacion_general_escolaridad))

# Desviación Estándar General
# Desviación Estándar General de Hijos: 1.20

# Interpretación: En promedio, el número de hijos por individuo varía 
# aproximadamente 1.20 hijos alrededor de la media de todos los individuos 
# la base de datos.


#Desviación Estándar General de Escolaridad (años): 2.66

#Interpretación: En promedio, los años de escolaridad por individuo varían 
# aproximadamente 2.66 años alrededor de la media de todos los individuos 
# en la base de datos.

# Calcular la desviación estándar por Estado Civil
desviacion_estado_civil <- base5ago %>%
  group_by(`Estado Civil`) %>%
  summarise(
    Desviacion_Hijos = sd(Hijos, na.rm = TRUE),
    Desviacion_Escolaridad = sd(`Escolaridad (años)`, na.rm = TRUE)
  )

# Calcular la desviación estándar por Género
desviacion_genero <- base5ago %>%
  group_by(Genero) %>%
  summarise(
    Desviacion_Hijos = sd(Hijos, na.rm = TRUE),
    Desviacion_Escolaridad = sd(`Escolaridad (años)`, na.rm = TRUE)
  )

# Calcular la desviación estándar por Estado Civil y Género
desviacion_estado_genero <- base5ago %>%
  group_by(`Estado Civil`, Genero) %>%
  summarise(
    Desviacion_Hijos = sd(Hijos, na.rm = TRUE),
    Desviacion_Escolaridad = sd(`Escolaridad (años)`, na.rm = TRUE)
  )

# Ver los resultados
print("Desviación Estándar por Estado Civil")
print(desviacion_estado_civil)

#Desviación Estándar por Estado Civil:
#Casado:
# Hijos: 1.27
# Escolaridad (años): 2.69
# Interpretación: Para las personas casadas, el número de hijos varía en 
# promedio 1.27 hijos alrededor de la media, y los años de escolaridad varían 
# en promedio 2.69 años alrededor de la media.

# Separado:
#  Hijos: 0.99
# Escolaridad (años): 2.56
# Interpretación: Para las personas separadas, el número de hijos varía en 
# promedio 0.99 hijos alrededor de la media, y los años de escolaridad 
# varían en promedio 2.56 años alrededor de la media.

#Soltero:
#Hijos: 1.11
#Escolaridad (años): 2.65
#Interpretación: Para las personas solteras, el número de hijos varía en 
#promedio 1.11 hijos alrededor de la media, y los años de escolaridad varían
#en promedio 2.65 años alrededor de la media.

print("Desviación Estándar por Género")
print(desviacion_genero)

#Desviación Estándar por Género:
#Hombre:
#Hijos: 1.07
#Escolaridad (años): 2.18
# Interpretación: Para los hombres, el número de hijos varía en promedio  
# 1.07 hijos alrededor de la media, y los años de escolaridad 
# varían en promedio 2.18 años alrededor de la media.

#Mujer:
# Hijos: 0.99
# Escolaridad (años): 2.56
# Interpretación: Para las mujeres, el número de hijos varía 
# en promedio 0.99 hijos alrededor de la media, y los años de 
# escolaridad varían en promedio 2.56 años alrededor de la media.

print("Desviación Estándar por Estado Civil y Género")
print(desviacion_estado_genero)

#Desviación Estándar por Estado Civil y Género:

#Casado Hombre:
#Hijos: 1.24
#Escolaridad (años): 1.99
#Interpretación: Para los hombres casados, el número de hijos varía en promedio
# 1.24 hijos alrededor de la media, y los años de escolaridad varían en 
#promedio 1.99 años alrededor de la media.

#Casado Mujer:
#Hijos: 1.06
#Escolaridad (años): 2.68
#Interpretación: Para las mujeres casadas, el número de hijos
#varía en promedio 1.06 hijos alrededor de la media, y los años 
#de escolaridad varían en promedio 2.68 años alrededor de la media.

#Separado Hombre:
# Hijos: 0.90
#Escolaridad (años): 2.12
#Interpretación: Para los hombres separados, el número de hijos 
#varía en promedio 0.90 hijos alrededor de la media, y los años
#de escolaridad varían en promedio 2.12 años alrededor de la media.

#Separado Mujer:

# Hijos: 0.66
#Escolaridad (años): 2.58
#Interpretación: Para las mujeres separadas, el número de hijos 
#varía en promedio 0.66 hijos alrededor de la media, y los años
#de escolaridad varían en promedio 2.58 años alrededor de la media.

#Soltero Hombre:

# Hijos: 0.99
#Escolaridad (años): 2.37
#Interpretación: Para los hombres solteros, el número de hijos 
#varía en promedio 0.99 hijos alrededor de la media, y los años
#de escolaridad varían en promedio 2.37 años alrededor de la media.

#Soltero Mujer:

# Hijos: 0.80
#Escolaridad (años): 2.35
#Interpretación: Para las mujeres solteras, el número de hijos
#varía en promedio 0.80 hijos alrededor de la media, y los años
#de escolaridad varían en promedio 2.35 años alrededor de la media.

# Resumen y Comparaciones

#Comparando entre Estados Civiles:
#Las personas casadas tienen una mayor variabilidad en el número de hijos
#y los años de escolaridad en comparación con las personas separadas y solteras.
#Las personas separadas tienen la menor variabilidad en el número de hijos, 
#lo que indica que sus datos son más consistentes en términos de cantidad de hijos.

# Comparando entre Géneros:
#Los hombres presentan una mayor variabilidad en el número de hijos 
# en comparación con las mujeres.
#Las mujeres tienen una mayor variabilidad en los años de escolaridad
#en comparación con los hombres.

#Comparando combinaciones de Estado Civil y Género:
#Las mujeres casadas tienen la mayor variabilidad en los años de escolaridad,
#mientras que las mujeres solteras tienen la menor variabilidad en el número de hijos.



#-----------------------------------------------------------------------------
# ¿ Qué es la varianza?
# El promedio de los cuadrados de las desviaciones medidas
# alrededor de la media. No es que cambie mucho los resultados anteriores
# ya que solamente se deben elevar al cuadrado

# Fórmula de la Varianza en la Muestra:
# s^2 = 1/(N-1) * sum((x_i - mean(x))^2)
#
# donde:
# s^2 = Varianza
# N = número de observaciones
# x_i = cada valor individual en el conjunto de datos
# mean(x) = media de los valores

# Fórmula de la Varianza en la Población:
# R^2 = 1/(N) * sum((x_i - mean(x))^2)
#
# donde:
# R^2 = Varianza
# N = número de observaciones
# x_i = cada valor individual en el conjunto de datos
# mean(x) = media de los valores
# Varianza general de Hijos y Escolaridad (años)

varianza_general_hijos <- var(base5ago$Hijos, na.rm = TRUE)
varianza_general_escolaridad <- var(base5ago$`Escolaridad (años)`, na.rm = TRUE)

# Imprimir resultados generales
print(paste("Varianza General de Hijos:", varianza_general_hijos))
print(paste("Varianza General de Escolaridad (años):", varianza_general_escolaridad))

# Calcular la varianza por Estado Civil
varianza_estado_civil <- base5ago %>%
  group_by(`Estado Civil`) %>%
  summarise(
    Varianza_Hijos = var(Hijos, na.rm = TRUE),
    Varianza_Escolaridad = var(`Escolaridad (años)`, na.rm = TRUE)
  )

# Calcular la varianza por Género
varianza_genero <- base5ago %>%
  group_by(Genero) %>%
  summarise(
    Varianza_Hijos = var(Hijos, na.rm = TRUE),
    Varianza_Escolaridad = var(`Escolaridad (años)`, na.rm = TRUE)
  )

# Calcular la varianza por Estado Civil y Género
varianza_estado_genero <- base5ago %>%
  group_by(`Estado Civil`, Genero) %>%
  summarise(
    Varianza_Hijos = var(Hijos, na.rm = TRUE),
    Varianza_Escolaridad = var(`Escolaridad (años)`, na.rm = TRUE)
  )

# Ver los resultados
print("Varianza por Estado Civil")
print(varianza_estado_civil)

print("Varianza por Género")
print(varianza_genero)

print("Varianza por Estado Civil y Género")
print(varianza_estado_genero)

# Interpretación:
# Varianza por Estado Civil:
#  Las personas casadas muestran una mayor variabilidad en el número de hijos 
#y los años de escolaridad en comparación con los separados y solteros.

#Varianza por Género:
# Los hombres tienen una mayor variabilidad en el número de hijos
#en comparación con las mujeres, mientras que las mujeres muestran
#una mayor variabilidad en los años de escolaridad.

#Varianza por Estado Civil y Género:
  
#Dentro de las personas casadas, los hombres tienen una mayor
#variabilidad en el número de hijos, mientras que las mujeres
#casadas tienen una mayor variabilidad en los años de escolaridad.
#Las mujeres separadas tienen la menor variabilidad en el número de hijos



#------------------------------------------------------------------------------
# El coeficiente de variación (CV):
# es una medida de dispersión relativa que expresa la desviación estándar
#como un porcentaje de la media. Es útil para comparar la variabilidad
#de diferentes datasets o variables con diferentes unidades de medida. 
#El CV se calcula como:

#CV = (Desviación Estandar/Media)*100

# Fórmula del Coeficiente de Variación
cv <- function(x) {
  (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100
}

mean(base5ago$`Escolaridad (años)`)
# Calcular el CV general de Hijos y Escolaridad (años)
cv_general_hijos <- cv(base5ago$Hijos)
cv_general_escolaridad <- cv(base5ago$`Escolaridad (años)`)

# Imprimir resultados generales
print(paste("Coeficiente de Variación General de Hijos:", cv_general_hijos))
print(paste("Coeficiente de Variación General de Escolaridad (años):", cv_general_escolaridad))

#Coeficiente de Variación General
#CV General de Hijos (81.94%):
  
# Este valor indica que la variabilidad en el número de hijos es alta
#en relación con la media. Es decir, el número de hijos varía considerablemente
#entre los individuos de la muestra.

#CV General de Escolaridad (18.02%):
  
# La variabilidad en los años de escolaridad es relativamente baja en
#comparación con el número de hijos. Esto sugiere que los años de 
#son más consistentes entre los individuos de la muestra.

# Calcular el CV por Estado Civil
cv_estado_civil <- base5ago %>%
  group_by(`Estado Civil`) %>%
  summarise(
    CV_Hijos = cv(Hijos),
    CV_Escolaridad = cv(`Escolaridad (años)`)
  )

# Calcular el CV por Género
cv_genero <- base5ago %>%
  group_by(Genero) %>%
  summarise(
    CV_Hijos = cv(Hijos),
    CV_Escolaridad = cv(`Escolaridad (años)`)
  )

# Calcular el CV por Estado Civil y Género
cv_estado_genero <- base5ago %>%
  group_by(`Estado Civil`, Genero) %>%
  summarise(
    CV_Hijos = cv(Hijos),
    CV_Escolaridad = cv(`Escolaridad (años)`)
  )

# Ver los resultados
print("Coeficiente de Variación por Estado Civil")
print(cv_estado_civil)

print("Coeficiente de Variación por Género")
print(cv_genero)

print("Coeficiente de Variación por Estado Civil y Género")
print(cv_estado_genero)

# Resumen de los resultados:
# Variabilidad en Hijos: Las personas solteras, especialmente las mujeres,
#muestran la mayor variabilidad en el número de hijos, lo que podría indicar
#una mayor diversidad en las decisiones reproductivas de este grupo.

#Variabilidad en Escolaridad: En general, la variabilidad en los años
#de escolaridad es baja en todos los grupos, lo que sugiere que la educación
#es más homogénea dentro de la muestra.

#------------------------------------------------------------------------------
#ANTES DE VER LA CURTOSIS, SE VAN HACER LAS GRÁFICAS DE 
# CADA SEGMENTACIÓN Y LOS CASOS GENERALES POR SUPUESTO:

# Histograma general para Hijos
hist(base5ago$Hijos, main = "Histograma General de Hijos", xlab = "Número de Hijos", col = "lightblue")

# Histograma general para Escolaridad
hist(base5ago$`Escolaridad (años)`, main = "Histograma General de Escolaridad", xlab = "Escolaridad (años)", col = "lightgreen")

# Crear tablas de frecuencias para Hijos por Estado Civil
hijos_casado <- table(cut(base5ago$Hijos[base5ago$`Estado Civil` == "Casado"], breaks = 5))
hijos_soltero <- table(cut(base5ago$Hijos[base5ago$`Estado Civil` == "Soltero"], breaks = 5))
hijos_separado <- table(cut(base5ago$Hijos[base5ago$`Estado Civil` == "Separado"], breaks = 5))

# Combinar en una matriz
hijos_freq_estado_civil <- rbind(hijos_casado, hijos_soltero, hijos_separado)
barplot(hijos_freq_estado_civil, beside = TRUE, col = c("lightblue", "lightcoral", "lightgreen"), legend.text = c("Casado", "Soltero", "Separado"), main = "Histograma de Hijos por Estado Civil", xlab = "Número de Hijos", ylab = "Frecuencia")

# Crear tablas de frecuencias para Escolaridad por Estado Civil
escolaridad_casado <- table(cut(base5ago$`Escolaridad (años)`[base5ago$`Estado Civil` == "Casado"], breaks = 5))
escolaridad_soltero <- table(cut(base5ago$`Escolaridad (años)`[base5ago$`Estado Civil` == "Soltero"], breaks = 5))
escolaridad_separado <- table(cut(base5ago$`Escolaridad (años)`[base5ago$`Estado Civil` == "Separado"], breaks = 5))

# Combinar en una matriz
escolaridad_freq_estado_civil <- rbind(escolaridad_casado, escolaridad_soltero, escolaridad_separado)
barplot(escolaridad_freq_estado_civil, beside = TRUE, col = c("lightblue", "lightcoral", "lightgreen"), legend.text = c("Casado", "Soltero", "Separado"), main = "Histograma de Escolaridad por Estado Civil", xlab = "Escolaridad (años)", ylab = "Frecuencia")

# Crear tablas de frecuencias para Hijos por Género
hijos_hombre <- table(cut(base5ago$Hijos[base5ago$Genero == "Hombre"], breaks = 5))
hijos_mujer <- table(cut(base5ago$Hijos[base5ago$Genero == "Mujer"], breaks = 5))

# Combinar en una matriz
hijos_freq_genero <- rbind(hijos_hombre, hijos_mujer)
barplot(hijos_freq_genero, beside = TRUE, col = c("lightblue", "lightpink"), legend.text = c("Hombre", "Mujer"), main = "Histograma de Hijos por Género", xlab = "Número de Hijos", ylab = "Frecuencia")

# Crear tablas de frecuencias para Escolaridad por Género
escolaridad_hombre <- table(cut(base5ago$`Escolaridad (años)`[base5ago$Genero == "Hombre"], breaks = 5))
escolaridad_mujer <- table(cut(base5ago$`Escolaridad (años)`[base5ago$Genero == "Mujer"], breaks = 5))

# Combinar en una matriz
escolaridad_freq_genero <- rbind(escolaridad_hombre, escolaridad_mujer)
barplot(escolaridad_freq_genero, beside = TRUE, col = c("lightblue", "lightpink"), legend.text = c("Hombre", "Mujer"), main = "Histograma de Escolaridad por Género", xlab = "Escolaridad (años)", ylab = "Frecuencia")

#------------------------------------------------------------------------------

# ¿Qué es la curtosis?

#La curtosis es una medida estadística que describe la "forma" de la
#distribución de los datos en términos de su cola o extremidad. 
#Básicamente, indica si los datos tienen colas más pesadas 
#o más ligeras que una distribución normal:

# Curtosis positiva (>3): Indica una distribución con colas más pesadas que
#la normal, conocida como leptocúrtica. Esto sugiere que hay más valores 
#extremos en los datos.

#Curtosis negativa (<3): Indica una distribución con colas más ligeras
#que la normal, conocida como platicúrtica. Esto sugiere menos valores extremos.

#Curtosis cercana a 3: Esto es similar a la distribución normal,
#conocida como mesocúrtica.

#install.packages("moments")
library(moments)

# Calcular la curtosis general de Hijos y Escolaridad (años)
curtosis_general_hijos <- kurtosis(base5ago$Hijos, na.rm = TRUE)
curtosis_general_escolaridad <- kurtosis(base5ago$`Escolaridad (años)`, na.rm = TRUE)

# Imprimir resultados generales
print(paste("Curtosis General de Hijos:", curtosis_general_hijos))
print(paste("Curtosis General de Escolaridad (años):", curtosis_general_escolaridad))

# La curtosis general para el número de hijos (2.61) y
#para los años de escolaridad (2.11) son ligeramente inferiores a 3,
#lo que indica que las distribuciones son ligeramente platicúrticas.
#Esto sugiere que los datos tienen colas más ligeras en comparación
#con una distribución normal, es decir, hay menos datos extremos (outliers)
#de lo que se esperaría en una distribución normal.

# Calcular la curtosis por Estado Civil
curtosis_estado_civil_hijos <- tapply(base5ago$Hijos, base5ago$`Estado Civil`, kurtosis, na.rm = TRUE)
curtosis_estado_civil_escolaridad <- tapply(base5ago$`Escolaridad (años)`, base5ago$`Estado Civil`, kurtosis, na.rm = TRUE)


# Convertir los resultados a data.frame para una mejor presentación
curtosis_estado_civil <- data.frame(
  `Estado Civil` = names(curtosis_estado_civil_hijos),
  Curtosis_Hijos = curtosis_estado_civil_hijos,
  Curtosis_Escolaridad = curtosis_estado_civil_escolaridad
)

print("Curtosis por Estado Civil")
print(curtosis_estado_civil)

# Curtosis de Hijos por Estado Civil
barplot(
  curtosis_estado_civil$Curtosis_Hijos,
  names.arg = curtosis_estado_civil$`Estado Civil`,
  main = "Curtosis de Hijos por Estado Civil",
  xlab = "Estado Civil",
  ylab = "Curtosis",
  col = "steelblue"
)

# Curtosis de Escolaridad por Estado Civil
barplot(
  curtosis_estado_civil$Curtosis_Escolaridad,
  names.arg = curtosis_estado_civil$`Estado Civil`,
  main = "Curtosis de Escolaridad por Estado Civil",
  xlab = "Estado Civil",
  ylab = "Curtosis",
  col = "coral"
)

# *Para las personas casadas, la curtosis de los hijos (3.09)
#es ligeramente superior a 3, indicando una distribución leptocúrtica,
#con colas más pesadas, lo que sugiere más presencia de valores extremos.

#*Para las personas solteras y separadas, la curtosis tanto
#en hijos como en escolaridad se mantiene cercana a 3,
#indicando distribuciones relativamente normales con alguna
#leve platicurtosis (menos valores extremos).



# Calcular la curtosis por Género
curtosis_genero_hijos <- tapply(base5ago$Hijos, base5ago$Genero, kurtosis, na.rm = TRUE)
curtosis_genero_escolaridad <- tapply(base5ago$`Escolaridad (años)`, base5ago$Genero, kurtosis, na.rm = TRUE)

# Convertir los resultados a data.frame para una mejor presentación
curtosis_genero <- data.frame(
  Genero = names(curtosis_genero_hijos),
  Curtosis_Hijos = curtosis_genero_hijos,
  Curtosis_Escolaridad = curtosis_genero_escolaridad
)

print("Curtosis por Género")
print(curtosis_genero)

# Curtosis de Hijos por Género
barplot(
  curtosis_genero$Curtosis_Hijos,
  names.arg = curtosis_genero$Genero,
  main = "Curtosis de Hijos por Género",
  xlab = "Género",
  ylab = "Curtosis",
  col = "darkgreen"
)

# Curtosis de Escolaridad por Género
barplot(
  curtosis_genero$Curtosis_Escolaridad,
  names.arg = curtosis_genero$Genero,
  main = "Curtosis de Escolaridad por Género",
  xlab = "Género",
  ylab = "Curtosis",
  col = "darkorange"
)

#La curtosis para el número de hijos entre las mujeres (5.40)
#es considerablemente alta, lo que indica una distribución leptocúrtica
#con colas más pesadas, sugiriendo una alta concentración de valores alrededor
#de la media y más valores extremos en comparación con los hombres.

#En términos de escolaridad, la curtosis es más alta para los hombres (4.11),
#indicando una distribución con colas más pesadas, mientras que para
#las mujeres, la distribución de la escolaridad es más cercana a la normalidad.


# Calcular la curtosis por Estado Civil y Género
curtosis_estado_genero_hijos <- tapply(base5ago$Hijos, list(base5ago$`Estado Civil`, base5ago$Genero), kurtosis, na.rm = TRUE)
curtosis_estado_genero_escolaridad <- tapply(base5ago$`Escolaridad (años)`, list(base5ago$`Estado Civil`, base5ago$Genero), kurtosis, na.rm = TRUE)

# Convertir los resultados a data.frame para una mejor presentación
curtosis_estado_genero <- data.frame(
  `Estado Civil` = rep(rownames(curtosis_estado_genero_hijos), times=ncol(curtosis_estado_genero_hijos)),
  Genero = rep(colnames(curtosis_estado_genero_hijos), each=nrow(curtosis_estado_genero_hijos)),
  Curtosis_Hijos = as.vector(curtosis_estado_genero_hijos),
  Curtosis_Escolaridad = as.vector(curtosis_estado_genero_escolaridad)
)



print("Curtosis por Estado Civil y Género")
print(curtosis_estado_genero)

# Curtosis de Hijos por Estado Civil y Género
barplot(
  curtosis_estado_genero$Curtosis_Hijos,
  names.arg = paste(curtosis_estado_genero$`Estado Civil`, curtosis_estado_genero$Genero, sep = "-"),
  main = "Curtosis de Hijos por Estado Civil y Género",
  xlab = "Estado Civil y Género",
  ylab = "Curtosis",
  col = "purple",
  las = 2 
)

# Curtosis de Escolaridad por Estado Civil y Género
barplot(
  curtosis_estado_genero$Curtosis_Escolaridad,
  names.arg = paste(curtosis_estado_genero$`Estado Civil`, curtosis_estado_genero$Genero, sep = "-"),
  main = "Curtosis de Escolaridad por Estado Civil y Género",
  xlab = "Estado Civil y Género",
  ylab = "Curtosis",
  col = "skyblue",
  las = 2 
)

#Hijos: Las mujeres solteras muestran una curtosis extremadamente alta (11.97),
#lo que sugiere una gran concentración de valores alrededor de la media
#y la presencia de valores extremos.


#Escolaridad: Los hombres separados presentan una curtosis alta
#tanto en hijos (3.75) como en escolaridad (4.46), indicando distribuciones
#con colas pesadas, es decir, hay una mayor frecuencia de valores extremos.



#-----------------------------------------------------------------------------
#¿ Qué es la Asimetria?

#La asimetría mide la falta de simetría en la distribución de los datos.
#Es un indicador de si los datos están sesgados hacia la izquierda 
#o la derecha de la media:
  
#Asimetría positiva (sesgo a la derecha): La cola derecha de la 
#distribución es más larga o está más dispersa que la izquierda.
#Indica que hay más valores bajos en el conjunto de datos,
#con algunos valores muy altos.


#Asimetría negativa (sesgo a la izquierda): La cola izquierda de la
#distribución es más larga o está más dispersa que la derecha.
#Indica que hay más valores altos en el conjunto de datos,
#con algunos valores muy bajos.


#Asimetría cercana a 0: Indica que la distribución es 
#aproximadamente simétrica, lo que significa que los datos
#están distribuidos uniformemente alrededor de la media.

# Asimetría general
asimetria_general_hijos <- skewness(base5ago$Hijos, na.rm = TRUE)
asimetria_general_escolaridad <- skewness(base5ago$`Escolaridad (años)`, na.rm = TRUE)

print(paste("Asimetría General de Hijos:", asimetria_general_hijos))
print(paste("Asimetría General de Escolaridad (años):", asimetria_general_escolaridad))

#La asimetría positiva indica que la distribución de la cantidad de hijos
#está sesgada hacia la derecha, lo que significa que hay un mayor número
#de individuos con pocos hijos y menos individuos con un número mayor de hijos.

#Similarmente, una asimetría positiva en los años de escolaridad
#indica que la mayoría de las personas tienen menos años de escolaridad,
#con algunos individuos con un mayor número de años de escolaridad.


# Asimetría por Estado Civil
asimetria_estado_civil <- base5ago %>%
  group_by(`Estado Civil`) %>%
  summarise(Asimetria_Hijos = skewness(Hijos, na.rm = TRUE),
            Asimetria_Escolaridad = skewness(`Escolaridad (años)`, na.rm = TRUE))

print("Asimetría por Estado Civil")
print(asimetria_estado_civil)

# Asimetría de Hijos por Estado Civil
barplot(asimetria_estado_civil$Asimetria_Hijos, names.arg = asimetria_estado_civil$`Estado Civil`,
        main = "Asimetría de Hijos por Estado Civil", xlab = "Estado Civil", ylab = "Asimetría",
        col = "lightblue")

# Asimetría de Escolaridad por Estado Civil
barplot(asimetria_estado_civil$Asimetria_Escolaridad, names.arg = asimetria_estado_civil$`Estado Civil`,
        main = "Asimetría de Escolaridad (años) por Estado Civil", xlab = "Estado Civil", ylab = "Asimetría",
        col = "lightgreen")

#Casado:
#   *Hijos: 0.946 - La fuerte asimetría positiva sugiere que, entre los casados,
#la mayoría tiene pocos hijos.

#   *Escolaridad: 0.217 - La asimetría positiva, aunque menor, indica una ligera
#tendencia a que los casados tienen un nivel educativo más bajo.


#Separado:
#     *Hijos: -0.144 - La asimetría negativa muestra que entre los separados,
#hay una ligera tendencia a que más personas tienen un número mayor de hijos.

#     *Escolaridad: 0.671 - La asimetría positiva indica que, en general,
#los separados tienen menos años de escolaridad, con unos pocos
#individuos con un nivel educativo más alto.


#Soltero:
#   *Hijos: 0.813 - La asimetría positiva indica que los solteros generalmente
#tienen pocos hijos.

#   *Escolaridad: 0.331 - La asimetría positiva, aunque pequeña, sugiere
#que los solteros tienden a tener menos años de escolaridad.


# Asimetría por Género
asimetria_genero <- base5ago %>%
  group_by(Genero) %>%
  summarise(Asimetria_Hijos = skewness(Hijos, na.rm = TRUE),
            Asimetria_Escolaridad = skewness(`Escolaridad (años)`, na.rm = TRUE))

print("Asimetría por Género")
print(asimetria_genero)

# Asimetría de Hijos por Género
barplot(asimetria_genero$Asimetria_Hijos, names.arg = asimetria_genero$Genero,
        main = "Asimetría de Hijos por Género", xlab = "Género", ylab = "Asimetría",
        col = "lightblue")

# Asimetría de Escolaridad por Género
barplot(asimetria_genero$Asimetria_Escolaridad, names.arg = asimetria_genero$Genero,
        main = "Asimetría de Escolaridad (años) por Género", xlab = "Género", ylab = "Asimetría",
        col = "lightgreen")

#Hombre:
#  Hijos: -0.026 - Casi simétrica, lo que indica que la distribución de la
#cantidad de hijos es bastante equilibrada entre los hombres.

#   Escolaridad: 1.15 - Una asimetría positiva considerable sugiere
#que muchos hombres tienen menos años de escolaridad,
#con algunos alcanzando niveles educativos más altos.


#Mujer:
#  Hijos: 1.53 - La asimetría positiva considerable indica que la mayoría
#de las mujeres tienen pocos hijos, con algunas teniendo muchos hijos.

#Escolaridad: -0.120 - Una ligera asimetría negativa sugiere que hay
#una tendencia hacia niveles más altos de escolaridad entre las mujeres.


# Asimetría por Estado Civil y Género
asimetria_estado_genero <- base5ago %>%
  group_by(`Estado Civil`, Genero) %>%
  summarise(Asimetria_Hijos = skewness(Hijos, na.rm = TRUE),
            Asimetria_Escolaridad = skewness(`Escolaridad (años)`, na.rm = TRUE))

print("Asimetría por Estado Civil y Género")
print(asimetria_estado_genero)

# Asimetría de Hijos por Estado Civil y Género
barplot(asimetria_estado_genero$Asimetria_Hijos, names.arg = paste(asimetria_estado_genero$`Estado Civil`, asimetria_estado_genero$Genero),
        main = "Asimetría de Hijos por Estado Civil y Género", xlab = "Estado Civil y Género", ylab = "Asimetría",
        col = "lightblue", las = 2)

# Asimetría de Escolaridad por Estado Civil y Género
barplot(asimetria_estado_genero$Asimetria_Escolaridad, names.arg = paste(asimetria_estado_genero$`Estado Civil`, asimetria_estado_genero$Genero),
        main = "Asimetría de Escolaridad (años) por Estado Civil y Género", xlab = "Estado Civil y Género", ylab = "Asimetría",
        col = "lightgreen", las = 2)

#Casado Hombre:
#  Hijos: -0.0783 - La ligera asimetría negativa indica que los hombres casados
#tienden a tener más hijos.
#Escolaridad: 0.973 - Asimetría positiva sugiere que los hombres casados tienen
#menos años de escolaridad, con algunos alcanzando niveles más altos.


#Casado Mujer:
#  Hijos: 1.40 - Asimetría positiva significativa indica que las mujeres 
#casadas tienden a tener pocos hijos.
#Escolaridad: 0.000587 - Casi simétrica, lo que indica una distribución
#equilibrada de la escolaridad entre las mujeres casadas.


#Separado Hombre:
#  Hijos: -0.461 - La asimetría negativa muestra que los hombres separados
#tienden a tener más hijos.
#Escolaridad: 1.19 - La asimetría positiva considerable sugiere que los hombres
#separados tienden a tener menos años de escolaridad.


#Separado Mujer:
#  Hijos: -0.097 - Casi simétrica, mostrando un equilibrio en la
#cantidad de hijos entre las mujeres separadas.
#Escolaridad: -0.579 - La asimetría negativa sugiere que
#las mujeres separadas tienden a tener más años de escolaridad.

#Soltero Hombre:
#  Hijos: 0.036 - Casi simétrica, indicando que la cantidad
#de hijos entre los hombres solteros es equilibrada.
#Escolaridad: 1.11 - Asimetría positiva indica que muchos
#hombres solteros tienen menos años de escolaridad.

#Soltero Mujer:
#  Hijos: 2.84 - La asimetría positiva significativa muestra
#que las mujeres solteras tienden a tener muy pocos hijos.
#Escolaridad: -0.084 - Casi simétrica, sugiriendo una distribución
#equilibrada de los años de escolaridad entre las mujeres solteras.

#------------------------------------------------------------------------------

# BOX PLOT FALTANTES :DDDDDD: 

boxplot(Hijos~`Estado Civil`,base5ago,
        horizontal =TRUE,
        col=c("pink","violet"))

boxplot(`Escolaridad (años)`~Genero,base5ago,
        horizontal =TRUE,
        col=c("pink","violet"))

boxplot(`Escolaridad (años)`~`Estado Civil`,base5ago,
        horizontal =TRUE,
        col=c("pink","violet"))


# Gracias por leer todo el código :DDDDDDD






