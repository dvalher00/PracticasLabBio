# Simulamos datos desde la distribución personalizada
set.seed(123)
rpower <- function(n, theta) runif(n)^(1/theta)
dpower <- function(x, theta) ifelse(x >= 0 & x <= 1, theta * x^(theta - 1), 0)

theta_true <- 2
x_data <- rpower(500, theta_true)

# Log-verosimilitud negativa
negloglik <- function(theta) {
  if (theta <= 0) return(Inf)
  -sum(log(dpower(x_data, theta)))
}

# Ajuste MLE usando optim
fit_mle <- optim(par = 1, fn = negloglik, method = "Brent", lower = 0.01, upper = 10)
theta_mle <- fit_mle$par
cat("Estimación MLE de theta:", theta_mle, "\n")
