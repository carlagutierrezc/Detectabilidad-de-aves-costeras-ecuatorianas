# Cargar paquetes necesarios 
library(dplyr)    # Para manipulación de datos (%>%, mutate, rename, etc.)
library(ggplot2)  # Para gráficos (próximas fases)
library(car)      # Para pruebas de supuestos (próximas fases)

setwd("C:/Users/usuario/Desktop/Proyecto Estadistica")
datos <- read.csv("aves_costa.csv")

# Calcular la mediana del tamaño para dicotomizar
mediana_tamano <- median(datos$tamano_promedio_cm, na.rm = TRUE)
cat("Mediana del tamaño:", mediana_tamano, "cm\n")

# Crear variable dicotomizada CON FACTORES ORDENADOS
datos <- datos %>%
  mutate(
    grupo_tamano = factor(
      ifelse(tamano_promedio_cm <= mediana_tamano, 
             "Aves Pequeñas", 
             "Aves Grandes"),
      levels = c("Aves Pequeñas", "Aves Grandes"),  # NIVELES ORDENADOS
      ordered = TRUE
    ),
    grupo_tamano_num = ifelse(tamano_promedio_cm <= mediana_tamano, 0, 1)
  )

# Verificar estructura del factor
cat("\n=== ESTRUCTURA DEL FACTOR ===\n")
str(datos$grupo_tamano)
cat("Niveles del factor:", levels(datos$grupo_tamano), "\n")

# Verificar distribución de grupos
cat("\n=== DISTRIBUCIÓN DE GRUPOS ===\n")
table(datos$grupo_tamano)
prop.table(table(datos$grupo_tamano))

cat("=== ESTADÍSTICOS DESCRIPTIVOS POR GRUPO ===\n")

estadisticos_grupos <- datos %>%
  group_by(grupo_tamano) %>%
  summarise(
    n = n(),
    Media = mean(numero_vocalizaciones, na.rm = TRUE),
    Mediana = median(numero_vocalizaciones, na.rm = TRUE),
    Desviacion = sd(numero_vocalizaciones, na.rm = TRUE),
    Minimo = min(numero_vocalizaciones, na.rm = TRUE),
    Maximo = max(numero_vocalizaciones, na.rm = TRUE),
    Q1 = quantile(numero_vocalizaciones, 0.25, na.rm = TRUE),
    Q3 = quantile(numero_vocalizaciones, 0.75, na.rm = TRUE)
  )

print(estadisticos_grupos)


cat("=== VISUALIZACIÓN COMPARATIVA ===\n")

# Boxplot para comparar distribuciones
boxplot_vocalizaciones <- ggplot(datos, aes(x = grupo_tamano, y = numero_vocalizaciones, fill = grupo_tamano)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Distribución de Vocalizaciones por Grupo de Tamaño",
       x = "Grupo de Tamaño",
       y = "Número de Vocalizaciones",
       fill = "Grupo") +
  theme_minimal() +
  theme(legend.position = "none")

print(boxplot_vocalizaciones)

# Histogramas separados por grupo
histogramas_grupos <- ggplot(datos, aes(x = numero_vocalizaciones, fill = grupo_tamano)) +
  geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
  facet_wrap(~ grupo_tamano, ncol = 1) +
  labs(title = "Distribución de Vocalizaciones por Grupo",
       x = "Número de Vocalizaciones",
       y = "Frecuencia",
       fill = "Grupo") +
  theme_minimal()

print(histogramas_grupos)

# Medidas resumen para interpretación
cat("\n=== MEDIDAS RESUMEN PARA INTERPRETACIÓN ===\n")
resumen_interpretacion <- datos %>%
  group_by(grupo_tamano) %>%
  summarise(
    Mediana = median(numero_vocalizaciones, na.rm = TRUE),
    Media = mean(numero_vocalizaciones, na.rm = TRUE),
    SD = sd(numero_vocalizaciones, na.rm = TRUE),
    CV = round((SD/Media)*100, 2)  # Coeficiente de variación
  )

print(resumen_interpretacion)

# Diferencia entre medianas
diff_medianas <- resumen_interpretacion$Mediana[2] - resumen_interpretacion$Mediana[1]
cat("Diferencia entre medianas (Grandes - Pequeñas):", diff_medianas, "\n")

# Análisis de tendencia visual
if(diff_medianas > 0) {
  cat("TENDENCIA VISUAL: Las aves grandes parecen tener MÁS vocalizaciones\n")
} else {
  cat("TENDENCIA VISUAL: Las aves pequeñas parecen tener MÁS vocalizaciones\n")
}


cat("=== PRUEBA DE NORMALIDAD (Kolmogorov-Smirnov) - HIPÓTESIS FORMALES ===\n\n")

cat("HIPÓTESIS PARA AVES PEQUEÑAS:\n")
cat("H₀: La variable 'numero_vocalizaciones' sigue una distribución normal\n")
cat("    en la población de Aves Pequeñas\n")
cat("H₁: La variable 'numero_vocalizaciones' NO sigue una distribución normal\n") 
cat("    en la población de Aves Pequeñas\n\n")

cat("HIPÓTESIS PARA AVES GRANDES:\n")
cat("H₀: La variable 'numero_vocalizaciones' sigue una distribución normal\n")
cat("    en la población de Aves Grandes\n")
cat("H₁: La variable 'numero_vocalizaciones' NO sigue una distribución normal\n")
cat("    en la población de Aves Grandes\n\n")

# Ejecutar pruebas KS
ks_pequenas <- ks.test(datos$numero_vocalizaciones[datos$grupo_tamano == "Aves Pequeñas"], "pnorm", 
                       mean = mean(datos$numero_vocalizaciones[datos$grupo_tamano == "Aves Pequeñas"], na.rm = TRUE),
                       sd = sd(datos$numero_vocalizaciones[datos$grupo_tamano == "Aves Pequeñas"], na.rm = TRUE))

ks_grandes <- ks.test(datos$numero_vocalizaciones[datos$grupo_tamano == "Aves Grandes"], "pnorm", 
                      mean = mean(datos$numero_vocalizaciones[datos$grupo_tamano == "Aves Grandes"], na.rm = TRUE),
                      sd = sd(datos$numero_vocalizaciones[datos$grupo_tamano == "Aves Grandes"], na.rm = TRUE))

# Resultados con interpretación formal
cat("=== RESULTADOS KOLMOGOROV-SMIRNOV ===\n\n")

cat("AVES PEQUEÑAS:\n")
cat("Estadístico D =", round(ks_pequenas$statistic, 4), "\n")
cat("Valor p =", ks_pequenas$p.value, "\n")

if(ks_pequenas$p.value < 0.05) {
  cat("DECISIÓN: Se RECHAZA H₀ (p < 0.05)\n")
  cat("CONCLUSIÓN: Existe evidencia estadística para afirmar que\n")
  cat("             el número de vocalizaciones NO sigue una distribución normal\n")
  cat("             en la población de Aves Pequeñas\n\n")
} else {
  cat("DECISIÓN: NO se rechaza H₀ (p ≥ 0.05)\n")
  cat("CONCLUSIÓN: No existe evidencia estadística para afirmar que\n")
  cat("             el número de vocalizaciones se desvía de la normalidad\n")
  cat("             en la población de Aves Pequeñas\n\n")
}

cat("AVES GRANDES:\n")
cat("Estadístico D =", round(ks_grandes$statistic, 4), "\n")
cat("Valor p =", ks_grandes$p.value, "\n")

if(ks_grandes$p.value < 0.05) {
  cat("DECISIÓN: Se RECHAZA H₀ (p < 0.05)\n")
  cat("CONCLUSIÓN: Existe evidencia estadística para afirmar que\n")
  cat("             el número de vocalizaciones NO sigue una distribución normal\n")
  cat("             en la población de Aves Grandes\n\n")
} else {
  cat("DECISIÓN: NO se rechaza H₀ (p ≥ 0.05)\n")
  cat("CONCLUSIÓN: No existe evidencia estadística para afirmar que\n")
  cat("             el número de vocalizaciones se desvía de la normalidad\n")
  cat("             en la población de Aves Grandes\n\n")
}

cat("\n=== PRUEBA DE HOMOGENEIDAD (Levene) ===\n\n")

# Prueba de Levene (robusta a no normalidad)
levene_test <- leveneTest(numero_vocalizaciones ~ grupo_tamano, data = datos)

cat("Prueba de Levene para homogeneidad de varianzas:\n")
cat("Estadístico F =", round(levene_test$`F value`[1], 4), "\n")
cat("Valor p =", levene_test$`Pr(>F)`[1], "\n\n")

# Interpretación
cat("INTERPRETACIÓN (Levene):\n")
if(levene_test$`Pr(>F)`[1] < 0.05) {
  cat("• LAS VARIANZAS SON SIGNIFICATIVAMENTE DIFERENTES (p < 0.05)\n")
  cat("• No se cumple el supuesto de homogeneidad de varianzas\n")
} else {
  cat("• LAS VARIANZAS SON HOMOGÉNEAS (p ≥ 0.05)\n")
  cat("• Se cumple el supuesto de homogeneidad de varianzas\n")
}


cat("=== PRUEBA U DE MANN-WHITNEY (UNILATERAL) ===\n\n")

cat("HIPÓTESIS FORMALES:\n")
cat("H₀: El número de vocalizaciones es igual o menor en aves grandes\n")
cat("    comparado con aves pequeñas (η_grandes ≤ η_pequeñas)\n")
cat("H₁: El número de vocalizaciones es mayor en aves grandes\n")
cat("    comparado con aves pequeñas (η_grandes > η_pequeñas)\n\n")

# Aplicar prueba U de Mann-Whitney UNILATERAL
prueba_mw <- wilcox.test(numero_vocalizaciones ~ grupo_tamano, 
                         data = datos,
                         alternative = "greater",  # PRUEBA UNILATERAL
                         exact = FALSE,           # Para muestras grandes
                         conf.int = TRUE)

cat("RESULTADOS PRUEBA U DE MANN-WHITNEY (UNILATERAL):\n")
cat("Estadístico W =", prueba_mw$statistic, "\n")
cat("Valor p =", prueba_mw$p.value, "\n")
cat("Diferencia de medianas =", prueba_mw$estimate, "\n")
cat("Intervalo de confianza 95%: [", prueba_mw$conf.int[1], ",", 
    prueba_mw$conf.int[2], "]\n\n")

# Interpretación formal
cat("INTERPRETACIÓN:\n")
if(prueba_mw$p.value < 0.05) {
  cat("DECISIÓN: Se RECHAZA H₀ (p < 0.05)\n")
  cat("CONCLUSIÓN: Existe evidencia estadística para afirmar que\n")
  cat("             el número de vocalizaciones es SIGNIFICATIVAMENTE MAYOR\n")
  cat("             en aves grandes comparado con aves pequeñas\n")
} else {
  cat("DECISIÓN: NO se rechaza H₀ (p ≥ 0.05)\n")
  cat("CONCLUSIÓN: No existe evidencia estadística para afirmar que\n")
  cat("             el número de vocalizaciones sea mayor en aves grandes\n")
  cat("             comparado con aves pequeñas\n")
}

# Verificar dirección de la diferencia
if(prueba_mw$estimate > 0) {
  cat("DIRECCIÓN: La diferencia favorece a AVES GRANDES\n")
} else {
  cat("DIRECCIÓN: La diferencia favorece a AVES PEQUEÑAS\n")
}
