if(!require("readxl")){install.packages("readxl")}
if(!require("pwr")){install.packages("pwr")}
if(!require("car")){install.packages("car")}
if(!require("MASS")){install.packages("MASS")}

library(readxl)
library(pwr)
library(car)
library(MASS)

insectos = read_xlsx("C:/Users/dvalh/Desktop/UA/MatesUA/Prácticas de empresa/Actividad 2/capturas_insectos_.xlsx")
summary(insectos) #Miro los datos

pwr.anova.test(k = 4, n = NULL, f = 0.4, sig.level = 0.05, power = 0.8)
#n = 15

insectos$Tratamiento = as.factor(insectos$Tratamiento)

modelo_aovtr = aov(Conteo ~ Tratamiento, data = insectos)
summary(modelo_aovtr)

#Normalidad
shapiro.test(resid(modelo_aovtr)) #No hay normalidad

#Homogeneidad
leveneTest(Conteo ~ Tratamiento, data = insectos) #Sí hay homogeneidad

#Transformo datos (si supongo que puedo añadir 0.00001)

insectos$Conteo_mod = insectos$Conteo+0.00001
boxcox_result <- boxcox(insectos$Conteo_mod ~ insectos$Tratamiento, lambda = seq(-4, 4, 0.1))

lambda_optimo <- boxcox_result$x[which.max(boxcox_result$y)]
print(lambda_optimo)

if(abs(lambda_optimo) < 0.01) {
  insectos$conteo_trans <- log(insectos$Conteo_mod + 1)  # +1 para evitar log(0)
} else {
  insectos$conteo_trans <- ((insectos$Conteo_mod + 1)^lambda_optimo - 1)/lambda_optimo
}

modelo_aov_box <- aov(conteo_trans ~ Tratamiento, data = insectos)
summary(modelo_aov_box)

shapiro.test(resid(modelo_aov_box)) #Hay normalidad
leveneTest(conteo_trans ~ Tratamiento, data = insectos) #Hay homogeneidad

#GLMs

if(!require("DHARMa")){install.packages("DHARMa")}
library(DHARMa)

#Cambiar nivel de referencia a Tratamiento = Control, quiero usarlo
#como baseline
insectos$Tratamiento <- factor(insectos$Tratamiento,
                                levels = c("Control", "A", "B", "A+B"))

#Gaussian

modelo_glm1 <- glm(Conteo ~ Tratamiento, 
                  family = gaussian(link = "identity"), 
                  data = insectos)

summary(modelo_glm1)

Anova(modelo_glm1)

# Simular residuos con DHARMa
residuos_simulados1 <- simulateResiduals(fittedModel = modelo_glm1)

# Gráficos de diagnóstico automáticos
plot(residuos_simulados1)

#Poisson

modelo_glm2 <- glm(Conteo ~ Tratamiento, 
                   family = poisson(link = "log"), 
                   data = insectos)

summary(modelo_glm2)

Anova(modelo_glm2)

# Simular residuos con DHARMa
residuos_simulados2 <- simulateResiduals(fittedModel = modelo_glm2)

# Gráficos de diagnóstico automáticos
plot(residuos_simulados2)

#Binomial negativa

modelo_glmnb <- glm.nb(Conteo ~ Tratamiento, data = insectos)

summary(modelo_glmnb)

Anova(modelo_glmnb)

# Simular residuos con DHARMa
residuos_simulados3 <- simulateResiduals(fittedModel = modelo_glmnb)

# Gráficos de diagnóstico automáticos
plot(residuos_simulados3)

#¡Este modelo es bueno! Me quedo con el binomial negativo

#Beta

if(!require("betareg")){install.packages("betareg")}
if(!require("statmod")){install.packages("statmod")}

library(betareg)
library(statmod)

# Escalado lineal al rango (0, 1)
insectos$conteo_esc <- (insectos$Conteo - min(insectos$Conteo)) / (max(insectos$Conteo) - min(insectos$Conteo))

# Ajustar valores 0 y 1
epsilon <- 1e-4
insectos$conteo_esc[insectos$conteo_esc == 0] <- epsilon
insectos$conteo_esc[insectos$conteo_esc == 1] <- 1 - epsilon

modelo_glmbeta <- betareg(conteo_esc ~ Tratamiento, data = insectos)

summary(modelo_glmbeta)

Anova(modelo_glmbeta)

residuos <- residuals(modelo_glmbeta, type = "pearson")
plot(residuos)

plot(fitted(modelo_glmbeta), residuos)
abline(h = 0, col = "red")

#Beta (para usar ks.test)

library(fitdistrplus)

# Ajustar beta a todo el conjunto de conteos escalados
fit_beta <- fitdist(residuos, "beta", method = "mle")

alpha <- fit_beta$estimate["shape1"]
beta <- fit_beta$estimate["shape2"]

my_cdf_beta <- function(q) {
  pbeta(q, shape1 = alpha, shape2 = beta)
}

ks.test(residuos, my_cdf_beta)

#Gamma

modelo_glm4 <- glm(Conteo_mod ~ Tratamiento,
                  family = Gamma(link = "log"),
                  data = insectos)

summary(modelo_glm4)

# Simular residuos con DHARMa
residuos_simulados4 <- simulateResiduals(fittedModel = modelo_glm4)

# Gráficos de diagnóstico automáticos
plot(residuos_simulados4)

#Cullen and Frey (detecta distribución)

if(!require("fitdistrplus")){install.packages("fitdistrplus")}
library(fitdistrplus)
descdist(insectos$Conteo, boot = 1000)

#glht con Tukey para la binomial negativa

if(!require("multcomp")){install.packages("multcomp")}
library(multcomp)
posthoc <- glht(modelo_glmnb, linfct = mcp(Tratamiento = "Tukey"))
summary(posthoc)
plot(posthoc)

#emmeans con Tukey para la binomial negativa

if (!require("emmeans")) install.packages("emmeans")
library(emmeans)

# Supongamos que ya tienes tu modelo glm.nb ajustado como modelo_glmnb
emmeans_trat <- emmeans(modelo_glmnb, ~ Tratamiento)
emmeans_trat

comparaciones <- pairs(emmeans_trat, adjust = "tukey")
comparaciones

if (!require("multcompView")) install.packages("multcompView")
library(multcompView)

cld(emmeans_trat, Letters = letters, adjust = "tukey") #Me muestra las letras

#Gráfico de modelos

if(!require("ggplot2")){install.packages("ggplot2")}

pos_letras <- data.frame(
  Tratamiento = factor(c("A+B", "A", "B", "Control"), levels = c("Control", "A", "B", "A+B")),
  ypos = c(280, 290, 300, 310),  # Ajusta si hay solapamiento
  Letters = c("a", "ab", "b", "b")
)

ggplot(insectos, aes(x = Tratamiento, y = Conteo, fill = Tratamiento)) +
  geom_boxplot(color = "black") +
  scale_y_continuous(limits = c(0, 320)) +
  scale_x_discrete(labels = c(
    "Control", "A", "B", "A+B")
  ) +
  scale_fill_manual(values = c(
    "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C"
  )) +
  labs(
    title = "Boxplot CONTEO",
    x = expression("Tratamiento"),
    y = "Conteo (uds.)"
  ) +
  geom_text(data = pos_letras, aes(x = Tratamiento, y = ypos-120, label = Letters), size = 5) +
  theme_minimal() +
  theme(legend.position = "none")
