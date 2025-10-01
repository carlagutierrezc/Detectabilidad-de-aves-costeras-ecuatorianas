# Cargar CSV
AVES_COSTA <- read.csv("AVES_COSTA.csv")

# Chequeo opcional de columnas
stopifnot(all(c("tamano_promedio_cm","numero_vocalizaciones") %in% names(AVES_COSTA)))

# Variable transformada (log1p = log(1 + x))
AVES_COSTA$log_vocalizaciones <- log1p(AVES_COSTA$numero_vocalizaciones)

# Estadísticos descriptivos
summary(AVES_COSTA$tamano_promedio_cm)
summary(AVES_COSTA$numero_vocalizaciones)
sd(AVES_COSTA$tamano_promedio_cm)
sd(AVES_COSTA$numero_vocalizaciones)

# Boxplots
boxplot(AVES_COSTA$numero_vocalizaciones,
        main = "N.º de vocalizaciones por hora", col = "aquamarine")
boxplot(AVES_COSTA$tamano_promedio_cm,
        main = "Tamaño Corporal Promedio (cm)", col = "bisque")

# Histogramas
hist(AVES_COSTA$tamano_promedio_cm,
     main = "Histograma de Tamaño Promedio (cm)",
     xlab = "Tamaño promedio (cm)", ylab = "Frecuencia",
     col = "bisque")
hist(AVES_COSTA$numero_vocalizaciones,
     main = "Histograma de N.º de vocalizaciones por hora",
     xlab = "N.º de vocalizaciones por hora", ylab = "Frecuencia",
     col = "aquamarine")
hist(AVES_COSTA$log_vocalizaciones,
     main = "Histograma de N.º de vocalizaciones (log1p)",
     xlab = "log(1 + N.º de vocalizaciones)",
     ylab = "Frecuencia",
     col = "lightblue", border = "black")

# Modelos lineales
modelo_original <- lm(numero_vocalizaciones ~ tamano_promedio_cm, data = AVES_COSTA)
modelo_log      <- lm(log_vocalizaciones ~ tamano_promedio_cm, data = AVES_COSTA)

# Dispersión + recta (original)
plot(x = AVES_COSTA$tamano_promedio_cm,
     y = AVES_COSTA$numero_vocalizaciones,
     xlab = "Tamaño promedio (cm)",
     ylab = "N.º de vocalizaciones por hora",
     main = "Relación: Tamaño Promedio vs N.º de vocalizaciones",
     pch = 1, col = "brown")
abline(modelo_original, col = "blue2", lwd = 2)
summary(modelo_original)

# Dispersión + recta (log)
plot(x = AVES_COSTA$tamano_promedio_cm,
     y = AVES_COSTA$log_vocalizaciones,
     xlab = "Tamaño promedio (cm)",
     ylab = "log(1 + N.º de vocalizaciones)",
     main = "Relación (transformada)",
     pch = 1, col = "brown")
abline(modelo_log, col = "blue2", lwd = 2)
summary(modelo_log)

# Normalidad de residuos
shapiro.test(residuals(modelo_original))
shapiro.test(residuals(modelo_log))

# Residuos vs Ajustados
plot(fitted(modelo_log), residuals(modelo_log),
     xlab = "Valores ajustados", ylab = "Residuos",
     main = "Residuos vs Ajustados (modelo log)")
abline(h = 0, col = "red")

plot(fitted(modelo_original), residuals(modelo_original),
     xlab = "Valores ajustados", ylab = "Residuos",
     main = "Residuos vs Ajustados (modelo original)")
abline(h = 0, col = "red")

