library(bootstrap)
data(law82)

lin_loocv <- function(data, i) {
  new_law <- data[-i, ]
  lin_model <- lm(GPA ~ LSAT, data = new_law)
  l <- predict(lin_model, newdata = data[i, ])
  l - law82$GPA[i]
}

lin_errors <- sapply(1:nrow(law82), lin_loocv, data = law82)
lin_mse <- mean(lin_errors^2)

quad_loocv <- function(data, i) {
  new_law <- data[-i, ]
  quad_model <- lm(GPA ~ poly(LSAT, degree = 2), data = new_law)
  q <- predict(quad_model, newdata = data[i, ])
  q - law82$GPA[i]
}

quad_errors <- sapply(1:nrow(law82), quad_loocv, data = law82)
quad_mse <- mean(quad_errors^2)


lin_mses <- rep(NA, 1000)
for(i in 1:1000) {
  boot_ind <- sample(1:nrow(law82), size = nrow(law82), replace = TRUE)
  law_boot <- law82[boot_ind, ]
  lin_mses[i] <- mean(sapply(1:nrow(law82), lin_loocv, data = law_boot)^2)
}

quad_mses <- rep(NA, 1000)
for(i in 1:1000) {
  boot_ind <- sample(1:nrow(law82), size = nrow(law82), replace = TRUE)
  law_boot <- law82[boot_ind, ]
  quad_mses[i] <- mean(sapply(1:nrow(law82), quad_loocv, data = law_boot)^2)
}

mean(lin_mses)
sd(lin_mses)
mean(quad_mses)
sd(quad_mses)

lin <- data.frame(lin_mses)
quad <- data.frame(quad_mses)
ggplot() + 
  geom_density(data = lin, aes(lin_mses), color = "blue", alpha = .3) +
  geom_density(data = quad, aes(quad_mses), color = "red", alpha = .3)
  