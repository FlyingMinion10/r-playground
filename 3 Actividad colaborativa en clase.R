###SESIÓN 6####
##### ANÁLISIS DE DATOS DE EXPRESIÓN DIFERENCIAL DE GENES  ##########
# 1. EXPLICACIÓN TEÓRICA #


##2. CHORD PLOT PARA TRAYECTORIAS DE GENES 

# Instalar y cargar librería
install.packages("circlize")
library(circlize)

# Definir los virus 
estados <- c("SARS", "COV2", "EBOLA")

# Relaciones entre los estados con la cantidad  de genes que se comparten
#Ejemplo: de Adapación a Viabilidad se encontraron 21 genes en común

relaciones <- data.frame(
  from = c("SARS", "SARS", "COV2", "SARS", "COV2", "EBOLA"), 
  to   = c("COV2", "EBOLA", "EBOLA", "Compartidos", "Compartidos", "Compartidos"), 
  value = c(21, 1041, 586, 630, 630, 630)  # Cantidad de genes compartidos
)

print(relaciones)
# Tamaño total de genes en cada estado para hacerlo más proporcional
tamanos <- c(SARS=1757, COV2=1287, EBOLA=5052, Compartidos=0)

# Configuraciones del chord plot
par(mar = rep(0, 4))  # Margen 

chordDiagram(x = relaciones, 
             grid.col = c("SARS" = "green", "COV2" = "blue", "EBOLA" = "darkorange", "Compartidos" = "darkred"),
             transparency = 0.5, 
             annotationTrack = "grid", #visualización de lineas de correspondencia
             preAllocateTracks = list(track.height = 0.05)) #preAllocateTracks = list(track.height = 0.05)
#Este parámetro se usa para preasignar el espacio que ocuparán las pistas en el gráfico (altura)


# Ajustar proporciones(ajuste de  la visualización y añadir etiquetas), REVISAR EN NOTAS EXTRAS

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 1)
}, bg.border = NA)

#NOTAS EXTRAS, CÓDIGO PARA ESTE TIPO DE GRÁFICO PARA FUTURAS APLICACIONES
#panel.fun= function (x,y) coordenadas en el espacio a graficar
#CELL_META$xcenter: Esta es la coordenada X del centro del sector actual (en el gráfico circular). CELL_META es un objeto que contiene metadatos sobre el sector en el que estamos trabajando, como su posición y el índice.

#CELL_META$ylim[1]: Esta es la coordenada Y en la parte inferior del sector actual. Es la posición inicial en el eje Y de cada sector, lo que ayuda a colocar el texto dentro de los límites del sector.

#CELL_META$sector.index: Este es el nombre del sector


#####CÓDIGO GENÉTICO##### ACTIVIDAD DE REVISIÓN EN CONJUNTO
# Código genético (codones -> aminoácidos)
codigo_genetico <- c(
  "UUU" = "Phe", "UUC" = "Phe", "UUA" = "Leu", "UUG" = "Leu",
  "UCU" = "Ser", "UCC" = "Ser", "UCA" = "Ser", "UCG" = "Ser",
  "UAU" = "Tyr", "UAC" = "Tyr", "UAA" = "STOP", "UAG" = "STOP",
  "UGU" = "Cys", "UGC" = "Cys", "UGA" = "STOP", "UGG" = "Trp",
  
  "CUU" = "Leu", "CUC" = "Leu", "CUA" = "Leu", "CUG" = "Leu",
  "CCU" = "Pro", "CCC" = "Pro", "CCA" = "Pro", "CCG" = "Pro",
  "CAU" = "His", "CAC" = "His", "CAA" = "Gln", "CAG" = "Gln",
  "CGU" = "Arg", "CGC" = "Arg", "CGA" = "Arg", "CGG" = "Arg",
  
  "AUU" = "Iso", "AUC" = "Iso", "AUA" = "Iso", "AUG" = "Met",
  "ACU" = "Thr", "ACC" = "Thr", "ACA" = "Thr", "ACG" = "Thr",
  "AAU" = "Asn", "AAC" = "Asn", "AAA" = "Lys", "AAG" = "Lys",
  "AGU" = "Ser", "AGC" = "Ser", "AGA" = "Arg", "AGG" = "Arg",
  
  "GUU" = "Val", "GUC" = "Val", "GUA" = "Val", "GUG" = "Val",
  "GCU" = "Ala", "GCC" = "Ala", "GCA" = "Ala", "GCG" = "Ala",
  "GAU" = "Asp", "GAC" = "Asp", "GAA" = "Glu", "GAG" = "Glu",
  "GGU" = "Gly", "GGC" = "Gly", "GGA" = "Gly", "GGG" = "Gly"
)

# Función para traducir una secuencia de ADN
traducirARN <- function(secuencia) {
  # Dividir la secuencia de ADN en codones de 3 bases
  codones <- substring(secuencia, seq(1, nchar(secuencia), by = 3), seq(3, nchar(secuencia), by = 3))
  
  # Traducir los codones a aminoácidos
  aminoacidos <- sapply(codones, function(codon) codigo_genetico[codon])
  
  # Filtrar los codones STOP
  aminoacidos <- aminoacidos[aminoacidos != "STOP"]
  
  return(aminoacidos)
}

# Secuencia de ARN
secuencia_arn <- "CUUGUUCGAAUUUAA"

# Traducir la secuencia a aminoácidos
aminoacidos <- traducirARN(secuencia_arn)

# Mostrar el resultado
cat("La secuencia de aminoácidos es:", paste(aminoacidos, collapse = "-"))


#######Secuencias de ARN ###





