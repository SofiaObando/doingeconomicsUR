# -*- coding: UTF-8 -*-  # Especifica la codificación de caracteres del script (UTF-8)
# -----------------------------------------------------------------------------
# Script Name: Analisis_Billeteras_Electronicas.R  #
# Creation Date: 2025-08-06 
# Author: Sofia Obando, Laura Valentina Corredor y Nathalie Arboleda 
# Description: 
#   Este script lee datos de una encuesta de tenderos desde un archivo Stata,
#   analiza el conocimiento y uso de diferentes billeteras electrónicas
#   (Daviplata, Nequi, Tpaga, Movii, Powwi, RappiPay, Tuya, BBVA Wallet, Otra).
#   Calcula el porcentaje de personas que usan cada billetera,
#   considerando únicamente respuestas válidas (0 y 1), y muestra
#   resultados en consola.
#
#   El análisis incluye:
#       - Carga de datos desde .dta
#       - Exploración inicial (estructura y resúmenes)
#       - Cálculo del % de uso para cada billetera
#
# Notes:
#   - Ajustar la ruta del archivo .dta antes de ejecutar el script.
#   - Los valores NA son excluidos del cálculo de porcentajes.
#
# References:
#   - Paquete haven: https://haven.tidyverse.org/
# -----------------------------------------------------------------------------

install.packages("haven")  # Instala el paquete 'haven' (si no está instalado) para leer archivos Stata
library(haven)  # Carga el paquete 'haven' en memoria para usar sus funciones

# ----------------------------
# 1. Cargar datos
# ----------------------------
datos <- read_dta("D:/Descargas/TenderosFU03_Publica.dta")  # Lee el archivo .dta y lo guarda en el objeto 'datos'

View(datos)  # Muestra la tabla de datos en una pestaña de RStudio para inspección visual
str(datos)   # Muestra la estructura del data frame (tipo de cada variable y ejemplos de valores)

# ----------------------------
# 2. Exploración inicial
# ----------------------------
summary(datos$Elec_Wallet_Knowledge)  # Resumen estadístico de la variable "Conoce billeteras electrónicas"
summary(datos$Elec_Wallet_Uso)        # Resumen estadístico de la variable "Alguna vez ha usado billetera electrónica"

# ----------------------------
# 3. Cálculo de porcentajes de uso por billetera
# ----------------------------

# ------------------ Daviplata ------------------
total_validosD <- sum(!is.na(datos$Elec_Wallet_Cual__1))  # Cuenta cuántos registros NO son NA
total_1sD <- sum(datos$Elec_Wallet_Cual__1 == 1, na.rm = TRUE)  # Cuenta cuántos valores son 1 (usó Daviplata)
porcentaje_1sD <- total_1sD / total_validosD * 100  # Calcula el % de uso sobre el total válido
cat("Total válidos (0 y 1) Daviplata:", total_validosD, "\n")  # Imprime total de datos válidos
cat("Total de 1s Daviplata:", total_1sD, "\n")  # Imprime total de usos (1s)
cat("Porcentaje de 1s Daviplata:", round(porcentaje_1sD, 2), "%\n")  # Imprime porcentaje redondeado a 2 decimales

# ------------------ Nequi ------------------
total_validosN <- sum(!is.na(datos$Elec_Wallet_Cual__2))  # Registros válidos para Nequi
total_1sN <- sum(datos$Elec_Wallet_Cual__2 == 1, na.rm = TRUE)  # Usos de Nequi
porcentaje_1sN <- total_1sN / total_validosN * 100  # % uso
cat("Total válidos (0 y 1) Nequi:", total_validosN, "\n")
cat("Total de 1s Nequi:", total_1sN, "\n")
cat("Porcentaje de 1s Nequi:", round(porcentaje_1sN, 2), "%\n")

# ------------------ Tpaga ------------------
total_validosT <- sum(!is.na(datos$Elec_Wallet_Cual__3))  # Registros válidos Tpaga
total_1sT <- sum(datos$Elec_Wallet_Cual__3 == 1, na.rm = TRUE)  # Usos Tpaga
porcentaje_1sT <- total_1sT / total_validosT * 100
cat("Total válidos (0 y 1) Tpaga:", total_validosT, "\n")
cat("Total de 1s Tpaga:", total_1sT, "\n")
cat("Porcentaje de 1s Tpaga:", round(porcentaje_1sT, 2), "%\n")

# ------------------ Movii ------------------
total_validosM <- sum(!is.na(datos$Elec_Wallet_Cual__4))  # Registros válidos Movii
total_1sM <- sum(datos$Elec_Wallet_Cual__4 == 1, na.rm = TRUE)  # Usos Movii
porcentaje_1sM <- total_1sM / total_validosM * 100
cat("Total válidos (0 y 1) Movii:", total_validosM, "\n")
cat("Total de 1s Movii:", total_1sM, "\n")
cat("Porcentaje de 1s Movii:", round(porcentaje_1sM, 2), "%\n")

# ------------------ Powwi ------------------
total_validosP <- sum(!is.na(datos$Elec_Wallet_Cual__5))  # Registros válidos Powwi
total_1sP <- sum(datos$Elec_Wallet_Cual__5 == 1, na.rm = TRUE)  # Usos Powwi
porcentaje_1sP <- total_1sP / total_validosP * 100
cat("Total válidos (0 y 1) Powwi:", total_validosP, "\n")
cat("Total de 1s Powwi:", total_1sP, "\n")
cat("Porcentaje de 1s Powwi:", round(porcentaje_1sP, 2), "%\n")

# ------------------ RappiPay ------------------
total_validosR <- sum(!is.na(datos$Elec_Wallet_Cual__6))  # Registros válidos RappiPay
total_1sR <- sum(datos$Elec_Wallet_Cual__6 == 1, na.rm = TRUE)  # Usos RappiPay
porcentaje_1sR <- total_1sR / total_validosR * 100
cat("Total válidos (0 y 1) RappiPay:", total_validosR, "\n")
cat("Total de 1s RappiPay:", total_1sR, "\n")
cat("Porcentaje de 1s RappiPay:", round(porcentaje_1sR, 2), "%\n")

# ------------------ Tuya ------------------
total_validosU <- sum(!is.na(datos$Elec_Wallet_Cual__7))  # Registros válidos Tuya
total_1sU <- sum(datos$Elec_Wallet_Cual__7 == 1, na.rm = TRUE)  # Usos Tuya
porcentaje_1sU <- total_1sU / total_validosU * 100
cat("Total válidos (0 y 1) Tuya:", total_validosU, "\n")
cat("Total de 1s Tuya:", total_1sU, "\n")
cat("Porcentaje de 1s Tuya:", round(porcentaje_1sU, 2), "%\n")

# ------------------ BBVA Wallet ------------------
total_validosB <- sum(!is.na(datos$Elec_Wallet_Cual__8))  # Registros válidos BBVA Wallet
total_1sB <- sum(datos$Elec_Wallet_Cual__8 == 1, na.rm = TRUE)  # Usos BBVA Wallet
porcentaje_1sB <- total_1sB / total_validosB * 100
cat("Total válidos (0 y 1) BBVA Wallet:", total_validosB, "\n")
cat("Total de 1s BBVA Wallet:", total_1sB, "\n")
cat("Porcentaje de 1s BBVA Wallet:", round(porcentaje_1sB, 2), "%\n")

# ------------------ Otra billetera ------------------
total_validosO <- sum(!is.na(datos$Elec_Wallet_Cual__9))  # Registros válidos para categoría "Otra"
total_1sO <- sum(datos$Elec_Wallet_Cual__9 == 1, na.rm = TRUE)  # Usos de otras billeteras
porcentaje_1sO <- total_1sO / total_validosO * 100
cat("Total válidos (0 y 1) Otro:", total_validosO, "\n")
cat("Total de 1s Otro:", total_1sO, "\n")
cat("Porcentaje de 1s Otro:", round(porcentaje_1sO, 2), "%\n")
