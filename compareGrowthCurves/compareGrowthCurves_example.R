# Instalar y cargar statmod si no lo tienes
if (!require(statmod)) install.packages("statmod")
library(statmod)

set.seed(123)

# Simular datos de crecimiento para 3 grupos, 10 individuos cada uno, 5 tiempos
n <- 3
times <- 3

# Crear grupos
groups <- factor(rep(c("A", "B", "C"), each = n))

# Simular curvas con diferencias reales entre grupos
# Grupo A: crecimiento lineal lento
matA <- t(sapply(1:n, function(x) 1 + 0.5 * (1:times) + rnorm(times, 0, 0.2)))
# Grupo B: crecimiento más rápido
matB <- t(sapply(1:n, function(x) 1 + 0.8 * (1:times) + rnorm(times, 0, 0.2)))
# Grupo C: crecimiento intermedio
matC <- t(sapply(1:n, function(x) 1 + 0.65 * (1:times) + rnorm(times, 0, 0.2)))

# Juntar datos en una matriz (filas = individuos, columnas = tiempos)
y <- rbind(matA, matB, matC)

# Usar compareGrowthCurves para comparar las 3 curvas
res <- compareGrowthCurves(group = groups, y = y, nsim = 1000, verbose = TRUE)

print(res)

# Paquetes para graficar
library(ggplot2)
library(dplyr)
library(tidyr)

# Preparar los datos para ggplot
df <- data.frame(
  Group = rep(groups, each = times),
  Time = rep(1:times, times = length(groups)),
  Growth = as.vector(t(y))
)

# Calcular medias y desviaciones por grupo y tiempo
summary_df <- df %>%
  group_by(Group, Time) %>%
  summarise(
    mean_growth = mean(Growth),
    sd_growth = sd(Growth),
    .groups = "drop"
  )

# Graficar medias y bandas de desviación
ggplot(summary_df, aes(x = Time, y = mean_growth, color = Group, fill = Group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = mean_growth - sd_growth, ymax = mean_growth + sd_growth), alpha = 0.2, color = NA) +
  geom_jitter(data = df, aes(x = Time, y = Growth, color = Group), width = 0.1, alpha = 0.5, inherit.aes = FALSE) +
  labs(title = "Curvas de Crecimiento Simuladas por Grupo",
       x = "Tiempo",
       y = "Crecimiento",
       color = "Grupo",
       fill = "Grupo") +
  theme_minimal(base_size = 15)

