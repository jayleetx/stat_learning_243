library(dplyr)
library(readr)
library(ggplot2)

source("http://raw.githubusercontent.com/andrewpbray/math-243/master/assets/week-11/pca-code.R")

https://raw.githubusercontent.com/andrewpbray/math-243/master/assets/data/handwritten.csv

d <- read_csv("http://raw.githubusercontent.com/andrewpbray/math-243/master/assets/data/handwritten.csv")

plot_letter <- function(x, hasletter = TRUE) {
  if(hasletter) {
    a <- as.numeric(x[, -1])
  }else{a <- as.numeric(x)}
  m <- matrix(a, nrow = 8, byrow = TRUE)
  m <- t(apply(m, 2, rev)) # rotate matrix
  par(mar = rep(0, 4))
  image(m, axes = FALSE, col = rev(grey(seq(0, 1, length = 256)))) #this should be a divergent palette
  box()
}

pc_grid <- function(pca, data) {
  d <- data
  grid_points <- as.matrix(expand.grid(seq(-1.5, 1.5, length.out = 5), 
                                       seq(-1.5, 1.5, length.out = 5)))
  pc_points <- pca$x[, 1:2]
  nearest_ind <- rep(NA, nrow(grid_points))
  for(i in 1:nrow(grid_points)) {
    gp <- matrix(rep(grid_points[i, ], nrow(pc_points)), 
                 ncol = 2, byrow = TRUE)
    nearest_ind[i] <- which.min(rowSums((pc_points - gp)^2))
  }
  nearest_grid <- data.frame(d[nearest_ind, ])
  par(mfrow = c(5, 5))
  regrid <- c(21:25, 16:20, 11:15, 6:10, 1:5)
  for(i in regrid) {
    plot_letter(nearest_grid[i, ])
  }
}



plot_letter(y[957, ])

y_mean <- colSums(y[ ,-1]) / nrow(y)
plot_letter(y_mean, hasletter = FALSE)

pca <- prcomp(y[ ,-1])

d <- data.frame(PC = 1:20,
                PVE = pca$sdev[1:20]^2 / sum(pca$sdev[1:20]^2))
ggplot(d, aes(x = PC, y = PVE)) +
  geom_line() + 
  geom_point()
