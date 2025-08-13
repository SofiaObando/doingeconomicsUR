# -*- coding: UTF-8 -*-
# -----------------------------------------------------------------------------
# Script Name: Analisis_Tiendas_Internet.R
# Creation Date: 2025-08-12
# Author: Sofia Obando, Laura Valentina Corredor, Nathalie Arboleda
# Description:
#   Este script procesa datos de tenderos y actividades comerciales,
#   calculando proporciones de acceso a internet por municipio y por actividad.
#   Genera tablas resumidas y gráficas facetadas para análisis y visualización.
#   Los datos provienen de:
#       - Archivo Stata: TenderosFU03_Publica.dta
#       - Archivo Excel: TerriData_Dim2_Sub1.xlsx
#
#   Basado en técnicas de manipulación de datos con dplyr y tidyr,
#   y visualización con ggplot2.
#
# Notes:
#   - Cargar solo los paquetes relevantes para optimizar memoria y tiempo.
#   - La ruta de trabajo debe ajustarse antes de ejecutar el script.
#   - Las salidas se exportan a un archivo Excel llamado "tablas_unidas.xlsx".
#
# References:
#   Documentación oficial de los paquetes usados:
#       https://dplyr.tidyverse.org/
#       https://ggplot2.tidyverse.org/
# -----------------------------------------------------------------------------
# ----------------------------
# 0. Instalación de paquetes necesarios
# ----------------------------
# Instala varios paquetes de una vez (solo es necesario la primera vez en tu PC)
# haven: para leer datos de Stata (.dta)
# readxl: para leer datos de Excel
# dplyr: para manipulación de datos (filtrar, agrupar, resumir, etc.)
# writexl: para exportar datos a Excel
# stringr, stringi: para manipulación de cadenas de texto
# tidyr: para reorganizar datos (pivotar, separar, etc.)
# ggplot2: para crear gráficos
install.packages(c("haven", "readxl", "dplyr", "writexl","readxl","stringr","tidyr","stringi","ggplot2"))

# ----------------------------
# 1. Cargar librerías
# ----------------------------
library(haven)    # Lectura de archivos .dta (Stata)
library(readxl)   # Lectura de archivos Excel
library(dplyr)    # Manipulación de datos
library(writexl)  # Exportar data frames a Excel
library(stringr)  # Manipulación avanzada de cadenas de texto
library(tidyr)    # Transformación y reestructuración de datos
library(stringi)  # Operaciones avanzadas con cadenas (similar a stringr)
library(ggplot2)  # Creación de gráficos

# ----------------------------
# 2. Cargar los datos originales
# ----------------------------
# tenderos: datos provenientes de Stata
tenderos <- read_dta("C:/Users/prestamour/Downloads/TenderosFU03_Publica.dta")
# territorios: datos desde Excel
territorios <- read_excel("C:/Users/prestamour/Downloads/TerriData_Dim2_Sub1.xlsx")

# ----------------------------
# 3. Procesamiento según Opción 1
# ----------------------------

# 3.1 Calcular total de tiendas y actividades por municipio
tiendas_totales <- tenderos %>%
  group_by(Munic_Dept) %>%  # Agrupa por código de municipio-departamento
  summarise(
    total_tiendas = n(),  # Cuenta el total de tiendas
    # Suma cada columna que empiece por "actG" (actividades)
    across(starts_with("actG"), ~sum(.x, na.rm = TRUE), .names = "total_{.col}")
  )

# 3.2 Calcular total de tiendas con internet por municipio
tiendas_internet <- tenderos %>%
  filter(uso_internet == 1) %>% # Filtra solo tiendas que usan internet
  group_by(Munic_Dept) %>%
  summarise(
    tiendas_con_internet = n(), # Cuenta las que tienen internet
    across(starts_with("actG"), ~sum(.x, na.rm = TRUE), .names = "internet_{.col}")
  )

# 3.3 Unir las dos tablas (totales e internet) por municipio
tabla_unida <- tiendas_totales %>%
  inner_join(tiendas_internet, by = "Munic_Dept")

# 3.4 Calcular proporción general de tiendas con internet (en %)
tabla_unida <- tabla_unida %>%
  mutate(
    prop_internet = (tiendas_con_internet / total_tiendas) * 100
  )

# 3.5 Calcular proporción por actividad (en %)
tabla_unida <- tabla_unida %>%
  mutate(
    across(
      .cols = internet_actG1:internet_actG11, # Selecciona todas las actividades internet
      # Divide las tiendas con internet en cada actividad entre el total de tiendas en esa actividad
      .fns = ~ (.x / tabla_unida[[sub("internet_", "total_", cur_column())]]) * 100,
      .names = "prop_{.col}" # Guarda con el prefijo "prop_"
    )
  )

# ----------------------------
# 4. Añadir nombres de municipios
# ----------------------------
# Crear tabla única con código y nombre de municipio
municipios_df <- tenderos %>%
  select(Munic_Dept, Municipio) %>% # Solo estas dos columnas
  distinct() # Quitar duplicados

# Unir con la tabla principal
tabla_unida <- tabla_unida %>%
  left_join(municipios_df, by = "Munic_Dept")

# Reordenar columnas para que Municipio esté después de Munic_Dept
tabla_unida <- tabla_unida %>%
  relocate(Municipio, .after = Munic_Dept)

# ----------------------------
# 5. Transformar a formato largo para Power BI
# ----------------------------
tabla_unida_larga <- tabla_unida %>%
  pivot_longer(
    cols = starts_with(c("total_actG", "internet_actG", "prop_internet_actG")),
    names_to = c("tipo", "actividad"),
    names_pattern = "(total|internet|prop_internet)_(actG\\d+)", # Separa en "tipo" y "actividad"
    values_to = "valor"
  ) %>%
  select(Munic_Dept, Municipio, tipo, actividad, valor, everything()) # Ordena columnas

# Ver las primeras filas para comprobar
head(tabla_unida_larga)

# ----------------------------
# 6. Exportar resultados a Excel
# ----------------------------
write_xlsx(
  list(
    tabla_unida = tabla_unida,           # Hoja 1: datos resumidos
    tabla_unida_larga = tabla_unida_larga # Hoja 2: datos en formato largo
  ),
  path = "tablas_unidas.xlsx"
)

# Ver la carpeta de trabajo actual
getwd()

# ----------------------------
# 7. Filtrar solo proporciones por actividad
# ----------------------------
prop_actividades <- tabla_unida_larga %>%
  filter(tipo == "prop_internet") # Solo filas con proporción de internet

# ----------------------------
# 8. Crear gráfico facetado por municipio (cambio de orientación)
# ----------------------------
# Vector con nombres descriptivos en orden
nombres_actividades <- c(
  "Tienda",
  "Comida preparada",
  "Peluqueria y belleza",
  "Ropa",
  "Otras variedades",
  "Papeleria y comunicaciones",
  "Vida nocturna",
  "Productos bajo inventario",
  "Salud",
  "Servicios",
  "Ferreteria y afines"
)

# Crear nueva columna con nombres descriptivos y ordenados
prop_actividades <- prop_actividades %>%
  mutate(
    nombre_actividad = factor(
      nombres_actividades[as.numeric(str_extract(actividad, "\\d+"))],
      levels = nombres_actividades
    )
  )

# Crear gráfico usando nombres descriptivos
ggplot(prop_actividades, aes(x = valor, y = nombre_actividad, fill = nombre_actividad)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = paste0(round(valor, 1), "%")),
    hjust = -0.1, size = 3
  ) +
  facet_wrap(~ Municipio) +
  labs(
    title = "Proporción de tiendas con internet por actividad en cada municipio",
    x = "Proporción (%)",
    y = "Actividad"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 8),
    legend.position = "none"
  )

# ----------------------------
# 9. Crear gráfico facetado por proporción de tiendas con internet
# ----------------------------

# Crear gráfico de barras con proporción de tiendas con internet por municipio
ggplot(tabla_unida, aes(x = reorder(Municipio, -prop_internet), y = prop_internet)) +
  
  # Barras con altura según la proporción y color azul específico
  geom_bar(stat = "identity", fill = "#2E86C1") +
  
  # Añadir etiquetas con el valor porcentual redondeado, justo a la izquierda de la barra
  geom_text(aes(label = paste0(round(prop_internet, 1), "%")),
            hjust = -0.1, color = "black", size = 3) +
  
  # Cambiar la orientación del gráfico: barras horizontales
  coord_flip() +
  
  # Etiquetas del gráfico: título, eje X (municipio), eje Y (proporción %)
  labs(
    title = "Proporción de tiendas con internet por municipio",
    x = "Municipio",
    y = "Proporción (%)"
  ) +
  
  # Tema minimalista para un diseño limpio
  theme_minimal()





