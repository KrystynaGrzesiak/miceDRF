
library(miceDRF)

set.seed(1001)
dat <- data.frame(MASS::mvrnorm(100, c(0, 0, 0, 0), diag(4)))
missdf <- dat
missdf[1:60, 1] <- NA

missdf[75:100, 2] <- NA

missdf[80:100, 3] <- NA


imp_fun <- impute_mic

Iscore()
