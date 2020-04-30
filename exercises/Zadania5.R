library(MASS)
Z <- c(0.26, 0.83, 6.04, 1.74, 0.18, 3.37, 3.60, 0.12, 1.23, 2.08)

# Zadanie 1:
fit <- fitdistr(Z, "exponential")
rate <- fit$estimate[[1]]
## Punkty i krzywa:
  curve(dexp(x, rate), col = 2, xlim = c(0, 10))
  points(Z, rep(0, 10), col = 1, pch = 19)

## Histogram gęstości:
  hist(Z, prob = TRUE)
  curve(dexp(x, rate), col = 2, add = TRUE)

## Wykres gęstości rozkładu wykładniczego:
  plot(ecdf(Z))
  curve(pexp(x, rate), col = 2, lwd = 2, add = TRUE)

## Wykres kwantyl-kwantyl:
  alpha = ppoints(30)
  my.qexp <- function(x) qexp(x, rate)
  qqplot(qexp(alpha, rate), Z)
  qqline(distribution = my.qexp, Z, col = 2, lwd = 2)

# Zadanie 2:
## Dopasowany wykres rozkładu weibulla
  fit <- fitdistr(Z, "weibull")
  shape <- fit$estimate[[1]]
  scale <- fit$estimate[[2]]
  plot(ecdf(Z))
  curve(pweibull(x, shape, scale), col = 2, lwd = 2, add = TRUE)

## Wykresy diagnostyczne
  ### Histogramy gęstości:
  hist(Z, prob = T)
  curve(dexp(x, rate), col = 2, lwd = 2, add = T)
  curve(dweibull(x, shape, scale), col = 3, lwd = 2, add = TRUE)

  ### Dystrybuanty:
  plot(ecdf(Z))
  fit <- fitdistr(Z, "exponential")
  rate <- fit$estimate[[1]]
  curve(pexp(x, rate), col = 2, lwd = 2, add = TRUE)
  fit <- fitdistr(Z, "weibull")
  shape <- fit$estimate[[1]]
  scale <- fit$estimate[[2]]
  curve(pweibull(x, shape, scale), col = 3, lwd = 2, add = TRUE)

## Wykresy kwantyl-kwantyl:
  alpha = ppoints(30)
  my.qweibull <- function(x) qweibull(x, shape, scale)
  qqplot(qweibull(alpha, shape, scale), Z)
  qqline(distribution = my.qexp, Z, col = 2, lwd = 2)
  qqline(distribution = my.qweibull, Z, col = 3, lwd = 2)

# Na podstawie powyższych wykresów nie jestem w stanie stwierdzić, która funkcja
# lepiej opisuje daną próbkę Z, obie funkcje opisują próbkę prawie identycznie
# i nie mogę powiedzieć, która robi to lepiej.