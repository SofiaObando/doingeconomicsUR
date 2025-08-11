install.packages("haven")
library(haven)

datos <- read_dta("C:\\Users\\natha\\OneDrive - Universidad del rosario\\URosario\\2025\\Décimo Semestre\\Haciendo Economía\\TenderosFU03_Publica.dta")

#Conoce las billeteras electrónicas?
summary(datos$Elec_Wallet_Knowledge)

#Alguna vez ha usado una billetera electrónica?
summary(datos$Elec_Wallet_Uso)

# % De personas que usan Daviplata
total_validosD <- sum(!is.na(datos$Elec_Wallet_Cual__1))
total_1sD <- sum(datos$Elec_Wallet_Cual__1 == 1, na.rm = TRUE)
porcentaje_1sD <- total_1sD / total_validosD * 100
cat("Total válidos (0 y 1) Daviplata:", total_validosD, "\n")
cat("Total de 1s Daviplata:", total_1sD, "\n")
cat("Porcentaje de 1s Daviplata:", round(porcentaje_1sD, 2), "%\n")

# % De personas que usan Nequi
total_validosN <- sum(!is.na(datos$Elec_Wallet_Cual__2))
total_1sN <- sum(datos$Elec_Wallet_Cual__2 == 1, na.rm = TRUE)
porcentaje_1sN <- total_1sN / total_validosN * 100
cat("Total válidos (0 y 1) Nequi:", total_validosN, "\n")
cat("Total de 1s Nequi:", total_1sN, "\n")
cat("Porcentaje de 1s Nequi:", round(porcentaje_1sN, 2), "%\n")

# % De personas que usan Tpaga
total_validosT <- sum(!is.na(datos$Elec_Wallet_Cual__3))
total_1sT <- sum(datos$Elec_Wallet_Cual__3 == 1, na.rm = TRUE)
porcentaje_1sT <- total_1sT / total_validosT * 100
cat("Total válidos (0 y 1)Tpaga:", total_validosT, "\n")
cat("Total de 1s Tpaga:", total_1sT, "\n")
cat("Porcentaje de 1s Tpaga:", round(porcentaje_1sT, 2), "%\n")

# % De personas que usan Movii
total_validosM <- sum(!is.na(datos$Elec_Wallet_Cual__4))
total_1sM <- sum(datos$Elec_Wallet_Cual__4 == 1, na.rm = TRUE)
porcentaje_1sM <- total_1sM / total_validosM * 100
cat("Total válidos (0 y 1) Movii:", total_validosM, "\n")
cat("Total de 1s Movii:", total_1sM, "\n")
cat("Porcentaje de 1s Movii:", round(porcentaje_1sM, 2), "%\n")

# % De personas que usan Powwi
total_validosP <- sum(!is.na(datos$Elec_Wallet_Cual__5))
total_1sP <- sum(datos$Elec_Wallet_Cual__5 == 1, na.rm = TRUE)
porcentaje_1sP <- total_1sP / total_validosP * 100
cat("Total válidos (0 y 1) Powwi:", total_validosP, "\n")
cat("Total de 1s Powwi:", total_1sP, "\n")
cat("Porcentaje de 1s Powwi:", round(porcentaje_1sP, 2), "%\n")

# % De personas que usan RappiPay
total_validosR <- sum(!is.na(datos$Elec_Wallet_Cual__6))
total_1sR <- sum(datos$Elec_Wallet_Cual__6 == 1, na.rm = TRUE)
porcentaje_1sR <- total_1sR / total_validosR * 100
cat("Total válidos (0 y 1) RappiPay:", total_validosR, "\n")
cat("Total de 1s RappiPay:", total_1sR, "\n")
cat("Porcentaje de 1s RappiPay:", round(porcentaje_1sR, 2), "%\n")

# % De personas que usan Tuya (Exito y Carulla)
total_validosU <- sum(!is.na(datos$Elec_Wallet_Cual__7))
total_1sU <- sum(datos$Elec_Wallet_Cual__7 == 1, na.rm = TRUE)
porcentaje_1sU <- total_1sU / total_validosU * 100
cat("Total válidos (0 y 1) Tuya:", total_validosU, "\n")
cat("Total de 1s Tuya:", total_1sU, "\n")
cat("Porcentaje de 1s Tuya:", round(porcentaje_1sU, 2), "%\n")

# % De personas que usan BBVA Wallet
total_validosB <- sum(!is.na(datos$Elec_Wallet_Cual__8))
total_1sB <- sum(datos$Elec_Wallet_Cual__8 == 1, na.rm = TRUE)
porcentaje_1sB <- total_1sB / total_validosB * 100
cat("Total válidos (0 y 1) BBVA Wallet:", total_validosB, "\n")
cat("Total de 1s BBVA Wallet:", total_1sB, "\n")
cat("Porcentaje de 1s BBVA Wallet:", round(porcentaje_1sB, 2), "%\n")

# % De personas que usan otra billetera
total_validosO <- sum(!is.na(datos$Elec_Wallet_Cual__9))
total_1sO <- sum(datos$Elec_Wallet_Cual__9 == 1, na.rm = TRUE)
porcentaje_1sO <- total_1sO / total_validosO * 100
cat("Total válidos (0 y 1) Otro:", total_validosO, "\n")
cat("Total de 1s Otro:", total_1sO, "\n")
cat("Porcentaje de 1s Otro:", round(porcentaje_1sO, 2), "%\n")