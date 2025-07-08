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

farmaconuevo2$trtf <- paste0(farmaconuevo2$trt, "_", farmaconuevo2$time, "h")

niveles_trtf <- c(
  paste0(niveles_trt, "_8h"),
  paste0(niveles_trt, "_24h")
)

farmaconuevo2$trtf <- factor(farmaconuevo2$trtf,
                                 levels = niveles_trtf,
                                 ordered = FALSE)

# Filtra los datos
subfarmaco8 = droplevels(subset(farmaconuevo2, time == 8))
subfarmaco24 = droplevels(subset(farmaconuevo2, time == 24))

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

#-------------INFERENCIA ESTADÍSTICA (ANOVA de un factor)-------------------

pwr.anova.test(k = 9, n = NULL, f = 0.4, sig.level = 0.05, power = 0.7)

#--Veo cómo son los datos----------------

str(farmaconuevo2)
table(farmaconuevo2$trtf)

summary(farmaconuevo2) #AQUÍ VEO SI HAY QUE HACER CLEANING DE LOS DATOS
                       #O HAY COSAS QUE NO ENCAJAN

#-----------------------
#Boxplots (abajo)
#-----------------------

modelo_aov <- aov(X._germinados ~ trtf, data = farmaconuevo2)
summary(modelo_aov)

shapiro.test(resid(modelo_aov))  # Normalidad
leveneTest(X._germinados ~ trtf, data = farmaconuevo2)  # Homogeneidad de varianzas

#FALLAN TANTO LA NORMALIDAD COMO LA HOMOGENEIDAD, TRANSFORMAMOS LOS DATOS

#----------Si ANOVA significativo--------------

TukeyHSD(modelo_aov)

EtaSq(modelo_aov, type = 1) #TAMAÑO EFECTO eta^2

#--------------------- TRANSFORMACIONES -------------------------

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

#Como no hay normalidad ni homogeneidad, no hacemos nada y usamos
#un test no paramétrico equivalente al ANOVA (Kruskal-Wallis + Dunn)

#Vería TukeyHSD 

tukeybox = TukeyHSD(modelo_aov_box)

#-------------INFERENCIA ESTADÍSTICA (ANOVA de dos factores)-------------------

# Convertimos f a f2 (para modelos generales)
f <- 0.4
f2 <- f^2  # f^2 = 0.16

# Grados de libertad para el numerador:
# k = número de grupos (combinaciones A x B) - 1
u <- 9 - 1  # = 8

# Potencia para detectar el efecto global (puede ser efecto principal o interacción)
pwr_result <- pwr.f2.test(u = u, f2 = f2, sig.level = 0.05, power = 0.8)

pwr_result


#-----------------------
#Boxplots (abajo)
#-----------------------

modelo_aov <- aov(X._germinados ~ trtf, data = farmaconuevo2)
summary(modelo_aov)

shapiro.test(resid(modelo_aov))  # Normalidad
leveneTest(X._germinados ~ trtf, data = farmaconuevo2)  # Homogeneidad de varianzas

#FALLAN TANTO LA NORMALIDAD COMO LA HOMOGENEIDAD, TRANSFORMAMOS LOS DATOS

#----------Si ANOVA significativo--------------

TukeyHSD(modelo_aov)

EtaSq(modelo_aov, type = 1) #TAMAÑO EFECTO eta^2

#--------------------- TRANSFORMACIONES -------------------------

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

#Como no hay normalidad ni homogeneidad, no hacemos nada y usamos
#un test no paramétrico equivalente al ANOVA (Kruskal-Wallis + Dunn)

#Vería TukeyHSD 

tukeybox = TukeyHSD(modelo_aov_box)

#################################

#-------------------------------- TESTS NO PARAMÉTRICOS Y BOXPLOT --------------------------------------

#---------------8 H-------------------------
modelo_kruskal = kruskal.test(X._germinados ~ trtf, data = subfarmaco8)
modelo_kruskal

if(!require("FSA", quietly = TRUE)){install.packages("FSA")}
if(!require("rcompanion", quietly = TRUE)){install.packages("rcompanion")}
if (!require("multcompView", quietly = TRUE)) install.packages("multcompView")

library(FSA)
library(rcompanion)

# COMPRUEBO OTRA VEZ SI trtf es factor (debe ser factor, no ordered factor)
class(farmaconuevo2$trtf)

# Test de Dunn con corrección de Bonferroni (puedes usar "holm" o "BH" también)
dunn_result <- dunnTest(X._germinados ~ trtf,
                        data = subfarmaco8,
                        method = "bonferroni")

print(dunn_result)

comparaciones <- dunn_result$res

#LETRAS

niveles_8h <- c("C_8h", "1_8h", "2.5_8h", "5_8h", "7.5_8h", "10_8h", "20_8h", "30_8h", "DMSO_35_8h")

library(multcompView)

# Formatear las comparaciones para multcompView
# multcompView necesita vector named con p-values y nombres del tipo "A-B"

# Convertir "A_8h - B_8h" a "A_8h-B_8h" para multcompView
names_pvals <- gsub(" ", "", gsub(" - ", "-", comparaciones$Comparison))

pvals <- comparaciones$P.adj
names(pvals) <- names_pvals

# Ahora generar el agrupamiento en letras
letters <- multcompLetters(pvals, threshold = 0.05)$Letters

# Ordenar letras según niveles_8h
letters_df <- data.frame(
  trtf = niveles_8h,
  Letters = letters[niveles_8h]
)
print(letters_df)

#GRÁFICOS

library(ggplot2)

#Gráfico

# Calcular las posiciones para las letras (por ejemplo, máximo + un poco)
library(dplyr)
pos_letras <- subfarmaco8 %>%
  group_by(trtf) %>%
  summarise(ypos = max(X._germinados, na.rm = TRUE) + 5)

# Añadimos las letras al dataframe de posiciones
pos_letras <- left_join(pos_letras, letters_df, by = "trtf")

# El gráfico con letras:
ggplot(subfarmaco8, aes(x = trtf, y = X._germinados, fill = trtf)) +
  geom_boxplot(color = "black") +
  scale_y_continuous(limits = c(0, 110), breaks=seq(0,100,20)) +
  scale_x_discrete(labels = c(
    "Control", "1", "2.5", "5", "7.5", "10", "20", "30",
    expression("DMSO (0.35%)")
  )) +
  scale_fill_manual(values = c(
    "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C",
    "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6"
  )) +
  labs(
    title = "Boxplot % Germinación por Tratamiento (8h)",
    x = expression("Tratamiento (" * mu * "g/ml)"),
    y = "% Germinación"
  ) +
  geom_text(data = pos_letras, aes(x = trtf, y = ypos, label = Letters), size = 5) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(subfarmaco8, aes(x = trtf, y = X._germinados, fill = trtf)) +
  geom_violin(trim = FALSE, color = "black") +   # Violinplot con borde negro
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_discrete(labels = c(
    "Control", "1", "2.5", "5", "7.5", "10", "20", "30",
    expression("DMSO (0.35%)")
  )) +
  scale_fill_manual(values = c(
    "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C",
    "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6"
  )) +
  labs(
    title = "Violinplot % Germinación por Tratamiento (8h)",
    x = expression("Tratamiento (" * mu * "g/ml)"),
    y = "% Germinación"
  ) +
  geom_text(data = pos_letras, aes(x = trtf, y = ypos+3, label = Letters), size = 5) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 12))

#---------------24 H-------------------------
modelo_kruskal2 = kruskal.test(X._germinados ~ trtf, data = subfarmaco24)
modelo_kruskal2

if(!require("FSA", quietly = TRUE)){install.packages("FSA")}
if(!require("rcompanion", quietly = TRUE)){install.packages("rcompanion")}
if (!require("multcompView", quietly = TRUE)) install.packages("multcompView")

library(FSA)
library(rcompanion)

# COMPRUEBO OTRA VEZ SI trtf es factor (debe ser factor, no ordered factor)
class(farmaconuevo2$trtf)

# Test de Dunn con corrección de Bonferroni (puedes usar "holm" o "BH" también)
dunn_result2 <- dunnTest(X._germinados ~ trtf,
                        data = subfarmaco24,
                        method = "bonferroni")

print(dunn_result2)

comparaciones2 <- dunn_result2$res

#LETRAS

niveles_24h <- c("C_24h", "1_24h", "2.5_24h", "5_24h", "7.5_24h", "10_24h", "20_24h", "30_24h", "DMSO_35_24h")

library(multcompView)

# Formatear las comparaciones para multcompView
# multcompView necesita vector named con p-values y nombres del tipo "A-B"

# Convertir "A_8h - B_8h" a "A_8h-B_8h" para multcompView
names_pvals2 <- gsub(" ", "", gsub(" - ", "-", comparaciones2$Comparison))

pvals2 <- comparaciones2$P.adj
names(pvals2) <- names_pvals2

# Ahora generar el agrupamiento en letras
letters2 <- multcompLetters(pvals2, threshold = 0.05)$Letters

# Ordenar letras según niveles_8h
letters_df2 <- data.frame(
  trtf = niveles_24h,
  Letters2 = letters2[niveles_24h]
)
print(letters_df2)

library(dplyr)
pos_letras2 <- subfarmaco24 %>%
  group_by(trtf) %>%
  summarise(ypos = max(X._germinados, na.rm = TRUE) + 5)

# Añadimos las letras al dataframe de posiciones
pos_letras <- left_join(pos_letras, letters_df, by = "trtf")

# El gráfico con letras:
ggplot(subfarmaco24, aes(x = trtf, y = X._germinados, fill = trtf)) +
  geom_boxplot(color = "black") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_discrete(labels = c(
    "Control", "1", "2.5", "5", "7.5", "10", "20", "30",
    expression("DMSO (0.35%)")
  )) +
  scale_fill_manual(values = c(
    "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C",
    "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6"
  )) +
  labs(
    title = "Boxplot % Germinación por Tratamiento (24h)",
    x = expression("Tratamiento (" * mu * "g/ml)"),
    y = "% Germinación"
  ) +
  geom_text(data = pos_letras, aes(x = trtf, y = ypos, label = Letters), size = 5) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(subfarmaco24, aes(x = trtf, y = X._germinados, fill = trtf)) +
  geom_violin(trim = FALSE, color = "black") +   # Violinplot con borde negro
  scale_y_continuous(limits = c(0, 110), breaks = seq(0,100,20)) +
  scale_x_discrete(labels = c(
    "Control", "1", "2.5", "5", "7.5", "10", "20", "30",
    expression("DMSO (0.35%)")
  )) +
  scale_fill_manual(values = c(
    "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C",
    "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6"
  )) +
  labs(
    title = "Violinplot % Germinación por Tratamiento (24h)",
    x = expression("Tratamiento (" * mu * "g/ml)"),
    y = "% Germinación"
  ) +
  geom_text(data = pos_letras, aes(x = trtf, y = ypos+3, label = Letters), size = 5) +
  theme_minimal() +
  theme(legend.position = "none")

