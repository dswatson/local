# Load libraries, register cores
library(mlbench)
library(dplyr)
library(doMC)
registerDoMC(8)

# Set seed
set.seed(123, kind = "L'Ecuyer-CMRG")

# Import data
data(BostonHousing)
data(BostonHousing2)

# Transform variables, rename response
dat <- BostonHousing %>%
  mutate(crim = log(crim), chas = as.numeric(chas) - 1,
         dis = log(dis), rad = log(rad), tax = log(tax),
         y = BostonHousing2$cmedv) %>%
  rename(black = b) %>%
  select(-medv)
n <- nrow(dat)
p <- ncol(dat) - 1


mc_fn <- function(b) {
  # Draw random subsample
  idx <- sample.int(n, n / 2)
  # Fit nested models
  delta_fn <- function(splt) {
    if (splt == 2) {
      idx <- seq_len(n)[-idx]
    }
    trn <- dat[-idx, ] 
    tst <- dat[idx, ]
    f1 <- lm(y ~ ., data = trn)
    yhat1 <- predict(f1, tst)
    loss1 <- (tst$y - yhat1)^2
    # Drop function
    drop_fn <- function(j) {
      f0 <- lm(y ~ ., data = trn[, -j])
      yhat0 <- predict(f0, tst)
      loss0 <- (tst$y - yhat0)^2
      delta <- loss0 - loss1
      out <- tibble(mean(delta))
      colnames(out) <- colnames(dat)[j]
      return(out)
    }
    out <- foreach(j = seq_len(p), .combine = cbind) %do% drop_fn(j)
    return(out)
  }
  out <- foreach(splt = c(1, 2), .combine = rbind) %do% delta_fn(splt)
  out$run <- b 
  return(out)
}
df <- foreach(b = seq_len(1e4), .combine = rbind) %dopar% mc_fn(b)

# Statistical test on each
sapply(seq_len(p), function(j) {
  t.test(df[[j]], alternative = 'greater')$p.value
})
sapply(seq_len(p), function(j) {
  binom.test(x = sum(df[[j]] > 0), n = 2e4, p = 0.5, 
             alternative = 'greater')$p.value
})
sapply(seq_len(p), function(j) {
  wilcox.test(df[[j]], alternative = 'greater')$p.value
})

# Clear verdict, any way you slice it: 
# crim, indus, and age are irrelevant







