#========================================
#CI Przedzialy ufnosci (confidence interval)
#======================================


#Zadanie 0
#=========
#Wyznacz kwantyle rzedu 1%, 5%, 95% oraz 99% rozkladow:
#a) N(0,1), N(0,5^2), N(100,5^2)
p <- c(0.01, 0.05, 0.95, 0.99)
res1 = qnorm(p)
res2 = qnorm(p, sd = 5)
res3 = qnorm(p, mean = 100, sd = 5)
res1; res2; res3

#b) t(4), t(10) #rozklad t-Studenta
res1 = qt(p, df = 4)
res2 = qt(p, df = 10)
res1; res2

#c) Exp(1), Exp(5), Exp(1/70), Exp(1/80) #rozklad wykladniczy
res1 = qexp(p)
res2 = qexp(p, rate = 5)
res3 = qexp(p, rate = 1 / 70)
res4 = qexp(p, rate = 1 / 80)
res1; res2; res3; res4

#Dana jest proba z rozkladu normalnego.
X = c(179, 180, 171, 177, 183, 167, 180, 182, 182, 178)
mu = mean(X); mu

#Zadanie 1 (rozwiazanie patrz EstymacjaPrzedzialowa.pdf, EstymacjaPrzedzialowa_wyklad)
#=========
#Napisz funkcje CI.mean1, obliczajaca konce przedzialu ufnosci dla sredniej,
#na poziomie ufnosci alpha, dla proby z rozkladu normalnego,
#przy sigma znanym (sigma=5)
#Wyznacz przedzialy na poziomie ufnosci 1-alpha=0.95 i 1-alpha=0.99 dla proby X.
#Oblicz dlugosci tych przedzialow.

CI.mean1 = function(data, sigma = 5, alpha = 0.05) {
  n = length(data)
  z = qnorm(1 - alpha / 2)
  xbar = mean(data)
  sdx = sigma / sqrt(n)
  l = xbar - z * sdx  #lewy koniec
  r = xbar + z * sdx  #prawy koniec
  return(c(l, r))
}

CI.mean1(X, 5, 0.05)
CI.mean1(X, 0.01)

#Zadanie 2
#=========
#Napisz funkcje CI.mean2, obliczajaca konce przedzialu ufnosci dla sredniej,
#dla proby z rozkladu normalnego, przy sigma nieznanym.
#Wyznacz przedzialy na poziomie ufnosci 0.95 i 0.99 dla proby X.
#Oblicz dlugosci tych przedzialow (porownaj z wynikami z Zadania 1).

CI.mean2 <- function(data, alpha = 0.05) {
  n = length(data)
  t = qt(1 - alpha / 2, n - 1)
  xbar = mean(data)
  sdx = sd(data) / sqrt(n)
  l = xbar - t * sdx
  r = xbar + t * sdx
  return(c(l, r))
}

CI.mean2(X)
CI.mean2(X, 0.01)

#Zadanie 3
#=========
#Napisz funkcje obliczajaca konce przedzialu ufnosci dla wariancji
#dla proby z rozkladu normalnego.
#Wyznacz przedzialy na poziomie ufnosci 0.95 i 0.99 dla proby X.
#Oblicz dlugosci tych przedzialow.

CI.Var <- function(data, alpha = 0.05) {
  n = length(data)
  top = var(data) * (n - 1)
  bottom.l = qchisq(1 - alpha / 2, n - 1)
  bottom.r = qchisq(alpha / 2, n - 1)
  l = top / bottom.l
  r = top / bottom.r
  return(c(l, r))
}

var(X)
CI.Var(X, 0.05)
CI.Var(X, 0.01)

#Zadanie 4
#=========
#Wczytaj plik Ceny_Akcji.csv (w RStudio w prawym górnym rogu: Environment, Import Dataset, From Text (base))
#a) Sprawdz na wykresach diagnostycznych, ze mozemy zalozyc normalnosc rozkladu cen kazdej z akcji.
#b) Wyznacz przedzialy ufnosci dla wartosci oczekiwanej i wariancji dla kazdej z akcji
#(wykorzystaj zdefiniowane funkcje).
library(MASS)
Ceny_Akcji = read.csv("Ceny_Akcji.csv") # getwd(), setwd()

diagnostic_plots <- function(X) {
  par(mfrow = c(2, 2))
  # Histogram:
  fit = fitdistr(X, "normal")
  mean = fit$estimate[[1]]
  sd = fit$estimate[[2]]
  hist(X, prob = T, ylim = c(0, 0.08))
  curve(dnorm(x, mean = mean, sd = sd), col = 2, lwd = 2, add = T)
  # Dystrybuanta:
  plot(ecdf(X))
  curve(pnorm(x, mean, sd), col = 2, lwd = 2, add = TRUE)
  # Kwantyl-kwantyl:
  alpha = ppoints(length(X))
  my.qnorm <- function(x) qnorm(x, mean, sd)
  qqplot(qnorm(alpha, mean, sd), X)
  qqline(distribution = my.qnorm, X, col = 2, lwd = 2)
}

# a) wykresy diagnostyczne
  diagnostic_plots(Ceny_Akcji$Akcje1)
  diagnostic_plots(Ceny_Akcji$Akcje2)
  diagnostic_plots(Ceny_Akcji$Akcje3)
# b)
  result <- data.frame(
    c(CI.mean2(Ceny_Akcji$Akcje1), CI.Var(Ceny_Akcji$Akcje1)),
    c(CI.mean2(Ceny_Akcji$Akcje2), CI.Var(Ceny_Akcji$Akcje2)),
    c(CI.mean2(Ceny_Akcji$Akcje3), CI.Var(Ceny_Akcji$Akcje3)))
  rownames(result) <- c(
    "Wartość oczekiwana [Początek]", "Wartość oczekiwana [Koniec]",
    "Wariancja [Początek]", "Wariancja [Koniec]")
  colnames(result) <- c("Akcje1", "Akcje2", "Akcje3")
  result