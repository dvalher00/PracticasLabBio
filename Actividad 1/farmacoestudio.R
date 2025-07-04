# 1. Cargar y preparar los datos
farmaco <- read.table("C:/Users/dvalh/Desktop/UA/MatesUA/Prácticas de empresa/estudiofarmaco.txt", sep = "\t", header = TRUE)

farmaconuevo1 <- na.omit(farmaco)

farmaconuevo2 <- farmaconuevo1  # Partimos de todos los datos sin filtrar

# Si X._germinados > 100, lo dividimos entre 10
farmaconuevo2$X._germinados <- ifelse(farmaconuevo2$X._germinados > 100,
                                      farmaconuevo2$X._germinados / 10,
                                      farmaconuevo2$X._germinados)

niveles_trt <- c("C", "1", "2.5", "5", "7.5", "10", "20", "30", "DMSO_35")
farmaconuevo2$trt <- factor(farmaconuevo2$trt, levels = niveles_trt, ordered = TRUE)

farmaconuevo2$trt_time <- paste0(farmaconuevo2$trt, "_", farmaconuevo2$time, "h")

niveles_trt_time <- c(
  paste0(niveles_trt, "_8h"),
  paste0(niveles_trt, "_24h")
)

farmaconuevo2$trt_time <- factor(farmaconuevo2$trt_time,
                                 levels = niveles_trt_time,
                                 ordered = TRUE)

# 2. Instalar y cargar paquetes necesarios
if (!require("pwr", quietly = TRUE)) install.packages("pwr")
if (!require("Superpower", quietly = TRUE)) install.packages("Superpower")
if (!require("car", quietly = TRUE)) install.packages("car")
if (!require("DescTools", quietly = TRUE)) install.packages("DescTools")
if (!require("ggplot2", quietly = TRUE)) install.packages("ggplot2")

library(pwr)
library(Superpower)
library(car)
library(DescTools)
library(ggplot2)

pwr.anova.test(k = 9, n = NULL, f = 0.4, sig.level = 0.05, power = 0.8)

str(farmaconuevo2)
table(farmaconuevo2$trt_time)

farmaconuevo2$trtf <- as.factor(farmaconuevo2$trt_time)

modelo_aov <- aov(X._germinados ~ trtf, data = farmaconuevo2)
summary(modelo_aov)

shapiro.test(resid(modelo_aov))  # Normalidad
leveneTest(X._germinados ~ trtf, data = farmaconuevo2)  # Homogeneidad de varianzas

#FALLAN TANTO LA NORMALIDAD COMO LA HOMOGENEIDAD, TRANSFORMAMOS LOS DATOS

#----------Si ANOVA significativo--------------

TukeyHSD(modelo_aov)

EtaSq(modelo_aov, type = 1) #TAMAÑO EFECTO eta^2

#----------------------------------------------

#PRIMERO PRUEBO CON UNA RAÍZ, POR EJEMPLO

farmaconuevo2$sqrt_germinados <- sqrt(farmaconuevo2$X._germinados)  # +1 para evitar log(0)
modelo_aov_sqrt <- aov(sqrt_germinados ~ trtf, data = farmaconuevo2)
summary(modelo_aov_sqrt)

shapiro.test(resid(modelo_aov_sqrt))
leveneTest(sqrt_germinados ~ trtf, data = farmaconuevo2)

#PRUEBO CON UNA TRANSFORMACIÓN BOX-COX QUE OPTIMIZA LOS DATOS PARA
#QUE LA DISTRIBUCIÓN DE LOS DATOS SEA MÁS NORMAL Y LOS RESIDUOS
#MÁS HOMOGÉNEOS

library(MASS)
boxcox_result <- boxcox(farmaconuevo2$X._germinados ~ farmaconuevo2$trtf, lambda = seq(-4, 4, 0.1))
# El gráfico te indicará el lambda óptimo

lambda_optimo <- boxcox_result$x[which.max(boxcox_result$y)]
print(lambda_optimo)

if(abs(lambda_optimo) < 0.01) {
  farmaconuevo2$Y_trans <- log(farmaconuevo2$X._germinados + 1)  # +1 para evitar log(0)
} else {
  farmaconuevo2$Y_trans <- ((farmaconuevo2$X._germinados + 1)^lambda_optimo - 1)/lambda_optimo
}

modelo_aov_box <- aov(Y_trans ~ trtf, data = farmaconuevo2)
summary(modelo_aov_box)

shapiro.test(resid(modelo_aov_box))
leveneTest(Y_trans ~ trtf, data = farmaconuevo2)

#Bajo p-valor de referencia a 0.01 y entonces se cumple 
#normalidad y homogeneidad

#Veo TukeyHSD

TukeyHSD(modelo_aov)

#GRÁFICOS
