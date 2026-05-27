# Leer el archivo CSV
Datos <- read.csv("C:/Users/tissera/Downloads/Base.csv", stringsAsFactors = FALSE)

# Ver las primeras filas
head(Datos)
#a) Analiza si el dataset presenta datos faltantes e indica el porcentaje para cada variable.
cat("\n--- Porcentaje de Datos Faltantes ---\n")
missing_pct <- Datos %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Porcentaje_NA") %>%
  arrange(desc(Porcentaje_NA))
print(missing_pct)

#b) Compare el continente y el nivel de ingreso de los países de manera 
# numérica y gráfica. Interprete.
cat("\n--- Tabla Cruzada: Continente vs Ingreso ---\n")
tabla_continente_ingreso <- table(Datos$continent, Datos$income_level)
print(tabla_continente_ingreso)

# Gráfico
ggplot(Datos, aes(x = continent, fill = income_level)) +
  geom_bar(position = "stack") +
  theme_minimal() +
  labs(title = "Nivel de Ingreso por Continente", x = "Continente", y = "Cantidad de Países", fill = "Nivel de Ingreso")

#c) ¿Qué continente tiene, en promedio, más plantas de energía? ¿Y cuál tiene menos?

cat("\n--- Promedio de Plantas de Energía por Continente ---\n")
plantas_por_continente <- aggregate(number_of_power_plants ~ continent, data = Datos, 
                                    FUN = mean, na.rm = TRUE)
colnames(plantas_por_continente)[2] <- "promedio_plantas"
print(plantas_por_continente[order(plantas_por_continente$promedio_plantas, decreasing = TRUE), ])


#d) Compare la cantidad de plantas de energía entre países con bajo (menos del 20%), 
#medio (entre 20% y 50%) y alto (más del 50%) porcentaje de energía renovable. 
#¿Qué diferencias observas?

# Crear variable con categorías explícitas incluyendo los NA
cat("\n--- Resumen: Plantas por Nivel de Energía Renovable ---\n")
library(dplyr)
library(ggplot2)

datos <- Datos %>%
  mutate(renovable_cat = case_when(
    is.na(renewable_energy_pct) ~ "Sin dato de energía renovable",
    renewable_energy_pct < 20 ~ "Bajo (<20%)",
    renewable_energy_pct >= 20 & renewable_energy_pct <= 50 ~ "Medio (20%-50%)",
    renewable_energy_pct > 50 ~ "Alto (>50%)"
  )) %>%
  mutate(renovable_cat = factor(renovable_cat,
                                levels = c("Bajo (<20%)", "Medio (20%-50%)", "Alto (>50%)", "Sin dato de energía renovable")))

# Resumen numérico (usar 'datos' no 'Datos')
resumen_plantas <- datos %>%
  group_by(renovable_cat) %>%
  summarise(
    media = mean(number_of_power_plants, na.rm = TRUE),
    mediana = median(number_of_power_plants, na.rm = TRUE),
    n = n()
  )
print(resumen_plantas)

# Gráfico de cajas
ggplot(datos, aes(x = renovable_cat, y = number_of_power_plants)) +
  geom_boxplot() +
  labs(x = "Porcentaje de energía renovable", y = "Número de plantas de energía",
       title = "Plantas de energía según uso de renovables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))  


library(dplyr)

# Función para detectar outliers según la regla del boxplot (1.5 * IQR)
detectar_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  limite_inferior <- q1 - 1.5 * iqr
  limite_superior <- q3 + 1.5 * iqr
  return(x < limite_inferior | x > limite_superior)
}

# Agregar columna que indica si el valor es atípico (por grupo)
datos <- datos %>%
  group_by(renovable_cat) %>%
  mutate(es_outlier = detectar_outliers(number_of_power_plants)) %>%
  ungroup()

# Crear tabla con los outliers
tabla_outliers <- datos %>%
  filter(es_outlier == TRUE) %>%
  select(pais = country, continente = continent, 
         porcentaje_renovable = renewable_energy_pct, 
         numero_plantas = number_of_power_plants, 
         categoria_renovable = renovable_cat) %>%
  arrange(categoria_renovable, numero_plantas)

# Mostrar la tabla
print(tabla_outliers)

# guardar como CSV
write.csv(tabla_outliers, "outliers_plantas_renovables.csv", row.names = FALSE)

#e) ¿Cómo describirías la distribución del ingreso por cápita?
#¿Es simétrica o sesgada? Fundamenta con medidas descriptivas y gráficas.

# 1. Limpieza de datos anómalos: Convertir valores negativos a NA
Datos <- Datos %>%
  mutate(gdp_per_capita = ifelse(gdp_per_capita < 0, NA, gdp_per_capita))

# 2. Medidas Descriptivas
cat("\n--- Medidas Descriptivas del Ingreso per Cápita (Limpios) ---\n")
mean_gdp <- mean(Datos$gdp_per_capita, na.rm = TRUE)
median_gdp <- median(Datos$gdp_per_capita, na.rm = TRUE)
skew_gdp <- skewness(Datos$gdp_per_capita, na.rm = TRUE)
cat(sprintf("Media: %.2f | Mediana: %.2f | Asimetría: %.3f\n", mean_gdp, median_gdp, skew_gdp))

# 3. Gráficos combinados (Histograma + Boxplot)
p1 <- ggplot(Datos, aes(x = gdp_per_capita)) + 
  geom_histogram(bins=20, fill="steelblue", alpha=0.7, color="black", na.rm = TRUE) + 
  theme_minimal() + labs(title = "Histograma de Ingreso", x = "Ingreso per Cápita", y="Frecuencia")

p2 <- ggplot(Datos, aes(y = gdp_per_capita)) + 
  geom_boxplot(fill="steelblue", alpha=0.7, na.rm = TRUE) + 
  theme_minimal() + labs(title = "Boxplot de Ingreso", y = "Ingreso per Cápita")

grid.arrange(p1, p2, ncol=2)

#f) ¿Cómo es la relación entre el 
#ingreso por cápita y las emisiones de CO₂? Visualiza la relación e interpreta.

summary(Datos$co2_emissions)
# --- Limpieza Inicial de Datos ---
# Limpiamos tanto el Ingreso per Cápita como las Emisiones de CO2
Datos <- Datos %>%
  mutate(
    gdp_per_capita = ifelse(gdp_per_capita < 0, NA, gdp_per_capita),
    co2_emissions = ifelse(co2_emissions < 0, NA, co2_emissions)
  )

# --- f) Ingreso per cápita vs Emisiones CO2 (Datos Limpios) ---
# El parámetro use = "complete.obs" le indica a cor() que solo use las filas
# que tienen datos válidos en AMBAS columnas.
cor_val <- cor(Datos$gdp_per_capita, Datos$co2_emissions, use = "complete.obs")

cat(sprintf("\n--- Correlación Ingreso - Emisiones (Limpios): %.2f ---\n", cor_val))

# Gráfico de Dispersión
ggplot(Datos, aes(x = gdp_per_capita, y = co2_emissions)) +
  # na.rm = TRUE evita el aviso en consola sobre los NA omitidos
  geom_point(alpha = 0.6, color = "darkred", size = 2, na.rm = TRUE) +
  theme_minimal() +
  labs(title = paste("Relación entre Ingreso per Cápita y Emisiones CO2 (r =", round(cor_val, 2), ")"), 
       x = "Ingreso per Cápita", 
       y = "Emisiones de CO2 (Toneladas/Cápita)")
