# ---
# title: "Zadania 7 - Estymacja przedziałowa, dowody"
# author: "JC"
# date: "28 kwietnia 2020"
# output: pdf_document
# ---
#Fakt 1
# Niech $X_1,X_2,\ldots,X_n$ będzie próbą prostą z rozkładu $N(\mu,\sigma^2)$. Zmienna losowa
# $$U={\overline X_n-\mu\over \sigma/\sqrt{n}}$$
# ma rozkład $N(0,1)$

#Przyklad 1.(Model 1)
#========
#Dla roznych mu i sigma, wygenerujemy N=1000 prob licznosci n=100: x1,x2,...,xn  z rozkładu N(mu,sigma^2)
#i korzystając z nich, wyznaczymy N: u1,u2,...,uN  mozliwych wartosci zmiennej U. Korzystajac z otrzymanej proby, sprawdzimy teze Faktu 1.
N=1000; n=100
m=3; sigma=2

licz.U <- function(dane) {(mean(dane)-m)/ (sigma/sqrt(n))}

U <- replicate(N, licz.U(rnorm(n,m,sigma)))

hist(U,prob=T)
curve(dnorm(x), add=T,col=2)

qqnorm(U)
abline(a=0,b=1,col=2)



#Fakt 2
# Niech $X_1,X_2,\ldots,X_n$ będzie próbą prostą z rozkładu $N(\mu,\sigma^2)$.
# Zmienna losowa
# $$t={\overline X_n-\mu\over S_n/\sqrt{n-1}},$$
# gdzie $S_n={1\over n}\sum_{i=1}^n(X_i-\overline X_n)^2$, ma rozkład t-Studenta o $n-1$ stopniach swobody.


#Zadanie 1.(Model 2)
#========
#Dla roznych mu i sigma, wygeneruj N=1000 prob licznosci n=100: x1,x2,...,xn  z rozkładu N(mu,sigma^2)
#i korzystajac z nich, wyznacz N: t1,t2,...,tN  mozliwych wartości zmiennej U. Korzystajac z testow diagnostycznych sprawdz teze Faktu 2.

N=1000; n=100
m=3; sigma=2

licz.t <- function(dane) {(mean(dane)-m)/ (sd(dane)/sqrt(n-1))}

t <- replicate(N, licz.t(rnorm(n,m,sigma)))

hist(t,prob=T)
curve(dt(x,n-1), add=T,col=2)

p <- ppoints(100); p
t.teor <- qt(p,n-1)

qqplot(t,t.teor)
abline(a=0,b=1,col=2)


#Fakt 3
# Dla próby  $X_1,X_2,\ldots,X_n$  z rozkładu $N(\mu,\sigma^2)$
# mamy kolejny ładny fakt. Zmienna losowa
# $$\chi^2={n S_n^2\over \sigma^2},$$
# gdzie $S_n^2={1\over n}\sum_{i=1}^n(X_i-\overline X_n)^2$, ma rozkład chi-kwadrat o $n-1$ stopniach swobody.


#Zadanie 2. (EPU dla wariancji)
#========
#Dla roznych mu i sigma, wygeneruj N=1000 prob licznosci n=100: x1,x2,...,xn  z rozkładu N(mu,sigma^2)
#i korzystajac z nich, wyznacz N: c1,c2,...,cn  mozliwych wartosci zmiennej chi^2. Korzystajac z testow diagnostycznych sprawdz teze Faktu 3.

N=1000; n=100
mu=3; sigma=2
licz.c <- function(dane) {(n*var(dane))/(sigma^2)}

c <- replicate(N, licz.c(rnorm(n,mu,sigma)))

hist(c,prob=T)
curve(dchisq(x,n-1), add=T,col=2)

p <- ppoints(n); p
c.teor <- qchisq(p,n-1)

qqplot(c,c.teor)
abline(a=0,b=1,col=2)

#Zadanie 3. (Model 3)
#========
#Wybierz trzy rozne rozklady (rozne tez od rozkladu normalnego) i wygeneruj z nich proby licznosci n=150: x1,x2,...,xn. Wyznacz przedzial ufnosci dla sredniej, korzystajac z Modelu 3. Sprawdz, czy wartosc oczekiwana rozkladu z ktorego generowana byla proba nalezy do wyliczonego przedzialu.
N=1000; n=150

CI.mean3 <- function(data, alpha = 0.05) {
  n = length(data)
  u = qnorm(1 - alpha / 2)
  xbar = mean(data)
  sdx = sd(data) / sqrt(n)
  l = xbar - u * sdx
  r = xbar + u * sdx
  return(c(l, r))
}

# rozkład wykładniczy
# E(X) = 1 / rate
rate = 5
X = rexp(n, rate)
curve(dexp(x, rate), col=2)
abline(v=1/rate, col="blue")
CImean = CI.mean3(X)
abline(v=CImean[1], col="purple")
abline(v=CImean[2], col="purple")

# rozkład chi kwadrat
# E(X) = df
df = 3
X = rchisq(n, df)
curve(dchisq(x, df), col=2, to=10)
abline(v=df, col="blue")
CImean = CI.mean3(X)
abline(v=CImean[1], col="purple")
abline(v=CImean[2], col="purple")

# rozkład jednorodny
# E(X) = (min + max) / 2
min = -5
max = 20
X = runif(n, min, max)
curve(dunif(x, min, max), col=2,from= -6, to=21)
abline(v=(min + max) / 2, col="blue")
CImean = CI.mean3(X)
abline(v=CImean[1], col="purple")
abline(v=CImean[2], col="purple")