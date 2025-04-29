#### Population trait distribution ####
k <- 10000              # Population size
mainland_opt <- 0       # Local optimum
max_dopt <- 10          # Max trait range expected
wopt <- max_dopt / 3    # Trait standard deviation (99.7% within range)

set.seed(123)
traits <- rnorm(k, mean = mainland_opt, sd = wopt)

#### Fitness function calibration ####
get_fitness <- function(x, opt, wmax, sigma) {
  wmax * exp(- ((x - opt)^2 / sigma^2))
}

t_x <- max_dopt/2-1      # Trait distance at which fitness = t_f
t_f <- 0.5               # Fitness value at distance t_x
sigma <- t_x / sqrt(-log(t_f))
wmax <- 1

# Fitness values for each individual
fitness_vals <- get_fitness(traits, opt = mainland_opt, wmax = wmax, sigma = sigma)

#### Plot both trait distribution and individual fitness ####
# Histogram and density
hist(traits, breaks = 40, col = rgb(173, 216, 230, max = 255, alpha = 150),
     main = "Trait distribution and individual fitness",
     xlab = "Trait value", ylab = "Density / Fitness",
     xlim = c(-12, 12), freq = FALSE,
     ylim = c(0, max(density(traits)$y, fitness_vals) * 1.2))

# Add density curve
lines(density(traits), col = "red", lwd = 2)

# Overlay fitness values for individuals
points(traits, fitness_vals, col = rgb(0, 0, 1, 0.3), pch = 16)

# Add vertical lines for optimum and thresholds
abline(v = mainland_opt, col = "darkgreen", lty = 2)
abline(v = c(mainland_opt - t_x, mainland_opt + t_x), col = "orange", lty = 2)

# Legend
legend("topright",
       legend = c("Trait density", "Individual fitness", "Optimum", paste("±", t_x)),
       col = c("red", "blue", "darkgreen", "orange"),
       lty = c(1, NA, 2, 2), pch = c(NA, 16, NA, NA), lwd = c(2, NA, 1, 1))

