# target is uniform
curve(dunif, xlab = "", xaxt = "n", ylab = "Probability density", 
      ylim = c(0, 2))

rnorm_trunc <- function(mean = .6, sd = .2, ll = 0, ul = 1) {
  x <- -1
  while (x > ul | x < ll) {
    x <- rnorm(1, mean, sd)
  }
  x
}

rnorm_trunc <- function(n, mean = 0, sd = 1, ll = 0, ul = 1) {
  x <- rnorm(n, mean, sd)
  while(any(x > ul | x < ll)) {
    invalid_x <- which(x > ul | x < ll)
    x[invalid_x] <- rnorm(length(invalid_x), mean, sd)
  }
  x
}

x1 <- rnorm_trunc()
points(x1, 0)
curve(dnorm(x, mean = x1, sd = 0.3), add = TRUE)

dnorm_trunc <- function(x, mean = 0, sd = 1, ll = 0, ul = 1) {
  if (x > ul | x < ll) {
    return(0)
  } else {
    dnorm(x, mean, sd) / (pnorm(ul, mean, sd) - pnorm(ll, mean, sd))
  }
}

ngrid <- 101
grid <- seq(0, 1, length.out = ngrid)
# Prior
prior_probs <- rep(1, ngrid)
# Update after data
update_probs <- function(x, prior_probs, pts = grid, sd = 0.2) {
  lik <- dnorm_trunc(x, pts, sd = sd)
  post_probs <- prior_probs * lik
  post_probs / sum(post_probs) / .01
}

compute_lik <- function(x, pts = grid, sd = 0.2) {
  lik_vals <- vapply(x, dnorm_trunc, mean = pts, sd = sd, 
                     FUN.VALUE = numeric(length(pts)))
  lik <- apply(lik_vals, 1, prod)
  lik / sum(lik) / .01
}

find_hdi_grid <- function(grid, dens, prob = .95) {
  dens <- dens / sum(dens)
  sorted_dens <- sort(dens, decreasing = TRUE)
  hdi_idx <- min(which(cumsum(sorted_dens) >= prob))
  hdi_height <- sorted_dens[hdi_idx]
  grid[range(which(dens >= hdi_height))]
}

library(tidyverse)
ggplot(tibble(x = grid, dens_x = prior_probs), 
       aes(x = x, y = dens_x)) + 
  geom_line() + 
  geom_vline(xintercept = find_hdi_grid(grid, prior_probs))