# Zadanie 1 P(X < 1) i P(X > 2)
# -----------------------------
  #a) X ~ N(0, 1)
  pn = pnorm(1); pn;
  pn = 1 - pnorm(2); pn;

  #b) X ~ Exp(1)
  pn = pexp(1); pn;
  pn = 1 - pexp(2); pn;


  #c) X ~ LN(0, 1).
  pn = plnorm(1); pn;
  pn = 1 - plnorm(2); pn;

# Zadanie 2
# ---------
  #a) P(X > 2), P(X > 3), P(1 < X < 2), P(2 < X < 3)
    # X ~ N(0, 1)
    pn = 1 - pnorm(2); pn;
    pn = 1 - pnorm(3); pn;
    pn = pnorm(2) - pnorm(1); pn;
    pn = pnorm(3) - pnorm(2); pn;

    # X ~ Exp(1)
    pn = 1 - pexp(2); pn;
    pn = 1 - pexp(3); pn;
    pn = pexp(2) - pexp(1); pn;
    pn = pexp(3) - pexp(2); pn;

    # X ~ LN(0, 1).
    pn = 1 - plnorm(2); pn;
    pn = 1 - plnorm(3); pn;
    pn = plnorm(2) - plnorm(1); pn;
    pn = plnorm(3) - plnorm(2); pn;

#b) Wektory
  # P(X > 2)
  pn1 <- round(c(1 - pnorm(2), 1 - pexp(2), 1 - plnorm(2)), digits = 2); pn1
  # P(X > 3)
  pn2 <- round(c(1 - pnorm(3), 1 - pexp(3), 1 - plnorm(3)), digits = 2); pn2
  # P(1 < X < 2)
  pn3 <- round(c(pnorm(2) - pnorm(1), pexp(2) - pexp(1), plnorm(2) - plnorm(1)), digits = 2); pn3
  # P(2 < X < 3)
  pn4 <- round(c(pnorm(3) - pnorm(2), pexp(3) - pexp(2), plnorm(3) - plnorm(2)), digits = 2); pn4

#c) Macierz
  pn = matrix(c(pn1, pn2, pn3, pn4), nrow = 3, ncol = 4); pn

# Zadanie 3 P(X > n) n <- 1:10
# ----------------------------
result = c()
for (i in 1:10) {
   pn <- c(1 - pnorm(i), 1 - pexp(i), 1 - plnorm(i))
   result = c(result, pn)
}

m <- matrix(round(result, digits = 2), nrow = 3, ncol = 10, byrow = FALSE); m
m <- matrix(round(result, digits = 2), nrow = 10, ncol = 3, byrow = TRUE); m

#Zadanie 4.
#----------
  #a) i b)
  par(mfrow = c(2,2))
  curve(dnorm(x, 1, 1), xlim = c(-4, 4), main = "N(1, 1)", xlab="x", ylab="y")
  curve(dnorm(x, 4, 1), xlim = c(-4, 4), main = "N(4, 1)", xlab="x", ylab="y")
  curve(dnorm(x, 0, 4), xlim = c(-4, 4), main = "N(0, 4)", xlab="x", ylab="y")
  curve(dnorm(x, 0, 9), xlim = c(-4, 4), main = "N(0, 9)", xlab="x", ylab="y")

#Zadanie 5.
#----------
X <- rnorm(1000, 0, 4^2)
Y <- exp(X);
hist(X, prob = TRUE)
curve(dlnorm(x, 0, 4^2), col = "red", add = TRUE)

#Zadanie 6.
#----------
X <- c()
y <- c()
for (sample in 1:1000) {
  sample <- rnorm(100, 0, 1)
  X <- c(X, sample)
  y <- c(y, sum(sample^2))
}
hist(y, prob = TRUE)
curve(dchisq(x, df=100), col = "red", add = TRUE)

#Zadanie 7.
#----------
curve(dnorm(x, 0, 1), col = "red", xlim = c(-4, 4), main = "N(0, 1) i t(4)", xlab="x", ylab="y")
curve(dt(x, 4),col = "blue", xlim = c(-4, 4), add = TRUE)
legend(2, 0.3, legend = c("N(0, 1)", "t(4)"), col=c("red", "blue"), lty=1, cex=1)

# Wykres t(4) ma grubsze ogony