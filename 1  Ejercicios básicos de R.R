# 1. Crea un vector x y luego imprímelo
x <- c(10, 11, 13, 1, 6, 3)
print(x)

# 2. Calcula estadísticas simples: media, desviación estándar y varianza,
# y guárdalas en un objeto llamado "vector_estadisticas"
vector_estadisticas <- c(
  media = mean(x),
  sd = sd(x),
  var = var(x)
)
print(vector_estadisticas)

# 3. Genera e imprime una secuencia de tercias del 3 al 33 (de 3 en 3)
secuencia <- seq(3, 33, by = 3)
print(secuencia)

# 4. Crea una matriz de 4 filas y 8 columnas con los números 1, 2, 3, 4 repetidos
matriz <- matrix(rep(c(1, 2, 3, 4), length.out = 32), nrow = 4, ncol = 8)
print(matriz)

# 5. Construye una base de datos (data frame) con la información de muestras de microorganismos
# Asumimos que la información se organiza en tres columnas: Alimento, Microorganismo y Toxicidad.
datos_micro <- data.frame(
  Alimento = c(1, 1, 3, 2, 1, 2, 3, 3),
  Microorganismo = c(100, 200, 450, 300, 480, 390, 820, 126),
  Toxicidad = c(1, 4, 8, 2, 9, 6, 3, 2)
)
print(datos_micro)

# 6. Genera un histograma de la columna "Toxicidad" y personaliza el color (por ejemplo, azul)
hist(datos_micro$Toxicidad,
     col = "blue",
     main = "Histograma de Toxicidad",
     xlab = "Toxicidad")

# 7. Crea una gráfica usando ggplot2 y personalízala (por ejemplo, un scatter plot de Microorganismo vs Toxicidad)
library(ggplot2)
ggplot(datos_micro, aes(x = Microorganismo, y = Toxicidad)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Toxicidad vs Microorganismo",
       x = "Microorganismo",
       y = "Toxicidad") +
  theme_minimal()

# 8. Si se tiene un archivo llamado "archivos para borrar", se podría eliminar con:
# Usando file.remove() para borrar un archivo o unlink() si es un directorio.
file.remove("archivos para borrar")  
# o, si se tratase de un directorio:
# unlink("archivos para borrar", recursive = TRUE)

# 9. Visualiza los primeros 4 conjuntos (filas) de la base de datos
head(datos_micro, 4)

# 10. Para borrar el historial de objetos (variables) de la sesión y ejecutar varias líneas simultáneamente:
# Borrar el entorno de trabajo (todas las variables)
rm(list = ls())

# Nota: En R, para ejecutar varias líneas de código a la vez se pueden seleccionar y ejecutar en conjunto (por ejemplo, con Ctrl+Enter en RStudio)
