# Función general para distribución bootstrap
bootstrap_distribution <- function(data, statistic, B = 10000, type = "nonparametric", dist = NULL, ...) {
  n <- length(data)
  t_boot <- numeric(B)
  
  if (type == "nonparametric") {
    for (b in 1:B) {
      boot_sample <- sample(data, size = n, replace = TRUE)
      t_boot[b] <- statistic(boot_sample, ...)
    }
  } else if (type == "parametric") {
    if (is.null(dist)) stop("Se requiere función de distribución")
    # Estimación paramétrica
    if (dist == "normal") {
      mu_hat <- mean(data)
      sigma_hat <- sd(data)
      for (b in 1:B) {
        boot_sample <- rnorm(n, mean = mu_hat, sd = sigma_hat)
        t_boot[b] <- statistic(boot_sample, ...)
      }
    } else if (dist == "gamma") {
      library(fitdistrplus)
      fit <- fitdist(data, "gamma")
      shape_hat <- fit$estimate["shape"]
      rate_hat <- fit$estimate["rate"]
      for (b in 1:B) {
        boot_sample <- rgamma(n, shape = shape_hat, rate = rate_hat)
        t_boot[b] <- statistic(boot_sample, ...)
      }
    }
  }
  
  return(t_boot)
}

# Ejemplo: Distribución bootstrap de la media
set.seed(123)
datos <- rgamma(100, shape = 2, rate = 1)  # Datos gamma

# 1. Bootstrap no paramétrico
boot_nonpar <- bootstrap_distribution(
  data = datos,
  statistic = mean,
  type = "nonparametric",
  B = 10000
)

# 2. Bootstrap paramétrico (gamma)
boot_par <- bootstrap_distribution(
  data = datos,
  statistic = mean,
  type = "parametric",
  dist = "gamma",
  B = 10000
)

# Visualización
par(mfrow = c(1, 2))
hist(boot_nonpar, breaks = 50, col = "skyblue", main = "Bootstrap No Paramétrico\nDistribución de la Media")
abline(v = mean(datos), col = "red", lwd = 2)

hist(boot_par, breaks = 50, col = "lightgreen", main = "Bootstrap Paramétrico\nDistribución de la Media")
abline(v = mean(datos), col = "red", lwd = 2)

# Estadísticos comparativos
cat("Media original:", mean(datos), "\n")
cat("Media bootstrap NP:", mean(boot_nonpar), "\n")
cat("Media bootstrap P:", mean(boot_par), "\n")
cat("Error estándar NP:", sd(boot_nonpar), "\n")
cat("Error estándar P:", sd(boot_par), "\n")

