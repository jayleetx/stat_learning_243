library(ISLR)
library(dplyr)
library(ggplot2)
data(Default)

set.seed(420)
test_ind <- sample(1:10000, size = 5000)
Default_test <- Default[test_ind, ]
Default_train <- Default[-test_ind, ]

est <- Default_train %>%
  group_by(default) %>%
  summarize(n = n(),
            prop = n/nrow(Default),
            mu = mean(balance),
            ssx = var(balance) * (n - 1))
est

pi_n <- as.numeric(est[1, 3])
pi_y <- as.numeric(est[2, 3])
mu_n <- as.numeric(est[1, 4])
mu_y <- as.numeric(est[2, 4])
sig_sq <- (1/(nrow(Default) - 2)) * sum(est$ssx)

my_lda <- function(x, pi, mu, sig_sq) {
  x * (mu/sig_sq) - (mu^2)/(2 * sig_sq) + log(pi)
}

d_n <- my_lda(Default_train$balance, pi_n, mu_n, sig_sq)
d_y <- my_lda(Default_train$balance, pi_y, mu_y, sig_sq)

balance <- seq(0, max(Default_train$balance), length.out = 50)
dx <- c(my_lda(balance, pi_n, mu_n, sig_sq), my_lda(balance, pi_y, mu_y, sig_sq))
default <- as.factor(rep(c("No", "Yes"), each = 50))
df <- data.frame(balance = rep(balance, 2), dx, default)
ggplot(df, aes(x = balance, y = dx, color = default)) +
  geom_line(lwd = 1.3) + 
  theme_bw()

Default_train <- mutate(Default_train, defaultYes = ifelse(default == "Yes", 1, 0))
m1 <- glm(default ~ balance, data = Default_train, family = binomial)

my_log_pred <- ifelse(m1$fit < 0.5, "No", "Yes")
my_lda_pred <- ifelse(d_n > d_y, "No", "Yes")

conf_lda <- table(my_lda_pred, Default_train$default)
conf_lda
conf_log <- table(my_log_pred, Default_train$default)
conf_log

(1/nrow(Default_train)) * (conf_lda[2, 1] + conf_lda[1, 2])
(1/nrow(Default_train)) * (conf_log[2, 1] + conf_log[1, 2])

# Test
d_n <- my_lda(Default_test$balance, pi_n, mu_n, sig_sq)
d_y <- my_lda(Default_test$balance, pi_y, mu_y, sig_sq)

my_log_pred <- ifelse(m1$fit < 0.5, "No", "Yes")
my_lda_pred <- ifelse(d_n > d_y, "No", "Yes")

conf_lda <- table(my_lda_pred, Default_test$default)
conf_lda
conf_log <- table(my_log_pred, Default_test$default)
conf_log

(1/nrow(Default_test)) * (conf_lda[2, 1] + conf_lda[1, 2])
(1/nrow(Default_test)) * (conf_log[2, 1] + conf_log[1, 2])
