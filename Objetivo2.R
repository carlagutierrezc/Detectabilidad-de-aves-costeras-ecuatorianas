# -----------------------------------------------
# CARGAR DATOS
# -----------------------------------------------
setwd("C:\\Users\\USER\\Documents\\Estadistica\\R")

# Verificar que el archivo existe
list.files(pattern = "\\.csv$")

# 1. Cargar el archivo CSV
AVES_COSTA <- read.csv("AVES_COSTA.csv")

# Verificar la estructura de los datos
str(AVES_COSTA)
head(AVES_COSTA)

# -----------------------------------------------
# ANÁLISIS PARA EL OBJETIVO 2
# -----------------------------------------------

# 1. Estadísticos descriptivos
summary(AVES_COSTA$duracion_canto_seg)
summary(AVES_COSTA$numero_avistamientos)
sd(AVES_COSTA$duracion_canto_seg)
sd(AVES_COSTA$numero_avistamientos)

# 2. Gráficos de caja (boxplots)
boxplot(AVES_COSTA$duracion_canto_seg, 
        main = "Duración del Canto (segundos)", 
        col = "lightcoral")

boxplot(AVES_COSTA$numero_avistamientos, 
        main = "Número de Avistamientos", 
        col = "lightgreen")

# 3. Histogramas
hist(AVES_COSTA$duracion_canto_seg, 
     main = "Histograma de Duración del Canto", 
     xlab = "Duración (segundos)", ylab = "Frecuencia", 
     col = "lightcoral")

hist(AVES_COSTA$numero_avistamientos, 
     main = "Histograma de Número de Avistamientos", 
     xlab = "Número de avistamientos", ylab = "Frecuencia", 
     col = "lightgreen")

# 4. Diagrama de dispersión
plot(AVES_COSTA$duracion_canto_seg, AVES_COSTA$numero_avistamientos,
     xlab = "Duración del canto (segundos)",
     ylab = "Número de avistamientos",
     main = "Relación entre Duración del Canto y Número de Avistamientos",
     pch = 19, col = "darkblue")

# 5. Prueba de normalidad (Shapiro-Wilk)
shapiro.test(AVES_COSTA$duracion_canto_seg)
shapiro.test(AVES_COSTA$numero_avistamientos)

# 6. Cálculo de correlación (Pearson o Spearman según normalidad)
# Si alguna variable no es normal, usamos Spearman
cor.test(AVES_COSTA$duracion_canto_seg, AVES_COSTA$numero_avistamientos, 
         method = "spearman") # o "pearson" si ambas son normales

# 7. Gráfico de correlación con línea de tendencia (opcional)
plot(AVES_COSTA$duracion_canto_seg, AVES_COSTA$numero_avistamientos,
     xlab = "Duración del canto (segundos)",
     ylab = "Número de avistamientos",
     main = "Correlación: Duración del Canto vs. Avistamientos",
     pch = 19, col = "darkblue")
abline(lm(numero_avistamientos ~ duracion_canto_seg, data = AVES_COSTA), 
       col = "red", lwd = 2)

