#### LIBRARY ####
library(deSolve)
library(ggplot2)
library(cowplot)

#### FUNCTIONS ####
get_fitness <- function(x, opt, wmax, sigma) {
  f <- wmax * exp( - ((x - opt)^2 / sigma^2))
  return(f)
} 
get_fitness <- function()

model <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    # Equations pour les traits
    dx_M <- sigma_g_M^2 * gamma_M * (theta_M - x_M) + m * (N_I / N_M) * (x_I - x_M)
    dx_I <- sigma_g_I^2 * gamma_I * (theta_I - x_I) + m * (N_M / N_I) * (x_M - x_I)
    
    # Equations pour les variances génétiques
    dV_M <- 2 * mu * V_M - gamma_M * V_M + m * (N_I / N_M) * (V_I - V_M)
    dV_I <- 2 * mu * V_I - gamma_I * V_I + m * (N_M / N_I) * (V_M - V_I)
    
    # Retourner les dérivées
    list(c(dx_M, dx_I, dV_M, dV_I))
  })
}

#### CODE ####
# Paramètres du modèle
parameters <- c(sigma_g_M = 0.1, 
                gamma_M = 0.1, 
                theta_M = 10, 
                sigma_g_I = 0.1, 
                gamma_I = 0.1, 
                theta_I = 10, 
                m = 0.01, 
                N_M = 5000, 
                N_I = 100, 
                mu = 0.01,
                wmax_M = 1,
                wmax_I = 1,
                sigma_M = 0.1,
                sigma_I = 0.1
                )
# SIGMA = FONCTION FITNESS "force" de la sélection stabilisante. Un grand σ signifie que la sélection favorise une gamme plus large de traits proches de l'optimum, tandis qu'un petit σ signifie que la sélection est plus stricte, ne favorisant que les individus avec des traits très proches de l'optimum.
# GAMMA = ED force de la sélection naturelle pour amener le trait génétique vers une valeur optimale. Si γ est élevé, la sélection ramène rapidement la population vers l'optimum, tandis qu'un γ faible signifie que la sélection est plus faible.
# Sigma détermine l'étendue de la courbe de fitness, c'est-à-dire à quelle distance de l'optimum un individu peut avoir un fitness relativement élevé. Gamma détermine la rapidité avec laquelle les traits de la population évoluent vers l'optimum.
# Conditions initiales

state <- c(x_M = 5, x_I = 5, V_M = 0.5, V_I = 0.5)

# Temps de simulation
times <- seq(0, 10000, by = 0.1)

# Résolution numérique
out <- ode(y = state, times = times, func = model, parms = parameters, atol = 1e-6, rtol = 1e-6)

# Convertir en dataframe pour une analyse facile
out <- as.data.frame(out)

# Calcul du fitness pour chaque population après la simulation
out$fitness_M <- get_fitness(out$x_M, parameters[3], parameters[11], parameters[13])
out$fitness_I <- get_fitness(out$x_I, parameters[6], parameters[12], parameters[14])

#### PLOTS ####
# Tracer l'évolution du fitness
plot.fitness.isl <- ggplot(out, aes(x = time)) +
  geom_line(aes(y = fitness_I, color = "Île Fitness"), size = 1) +
  labs(x = "Temps", y = "Fitness", color = "Population") +
  theme_minimal() +
  scale_color_manual(values = c("blue"), labels = c("Île")) +
  theme(legend.position = "top")

plot.fitness.main <- ggplot(out, aes(x = time)) +
  geom_line(aes(y = fitness_M, color = "Continent Fitness"), size = 1) +
  labs(x = "Temps", y = "Fitness", color = "Population") +
  theme_minimal() +
  scale_color_manual(values = c("red"), labels = c("Continent")) +
  theme(legend.position = "top")

plot_grid(plot.fitness.isl,plot.fitness.main,nrow=1,ncol=2)

# Tracer l'évolution des traits moyens et des variances
plot.variance.isl <- ggplot(out, aes(x = time)) +
  geom_line(aes(y = x_I, color = "Île x"), size = 1) +
  geom_line(aes(y = V_I, color = "Île Variance"), size = 1) +
  labs(x = "Temps", y = "Trait / Variance", color = "Légende") +
  theme_minimal()

plot.variance.main <- ggplot(out, aes(x = time)) +
  geom_line(aes(y = x_M, color = "Continent x"), size = 1) +
  geom_line(aes(y = V_M, color = "Continent Variance"), size = 1) +
  labs(x = "Temps", y = "Trait / Variance", color = "Légende") +
  theme_minimal()

plot_grid(plot.variance.isl,plot.variance.main,nrow=1,ncol=2)



