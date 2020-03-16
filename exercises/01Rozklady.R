#=====================
#R: podstawowe operacje
#=====================
#Liczby - zaokraglenia
##Cwiczenie 1.  Sprawdz dzialanie ponizszych  funkcji dla x=0.6789
x <- 0.6789
ceiling(x)
floor(x)
trunc(x, ...)
round(x, digits = 2)

#Wektory
#Cwiczenie 2. Sprawdz na przykladach dzialanie operatora ":".
1:10;
10:-10

#Cwiczenie 3. Sprawdz na przykladach dzialanie funkcji "seq".
seq(-10,10, by=2)
seq(-10,10, length=50)


#Cwiczenie 4. Sprawdz na przykladach dzialanie funkcji "rep".
rep(1:3,5)
rep(1:3,length.out=17)
rep(1:3,rep(5,3))


#Indeksowanie wektorow
#Cwiczenie 5. Jak wywolujemy wyrazy wektora? Sprawdz na przykladach.
a=seq(-10,10,by=2); a
a[3]
a[c(4,6,8)]
a[1:5]
#Sprawdz co bedzie jesli w nawiasie beda liczby ujemne
#lub operator logiczny.
a[-3]
a[-c(4,6,8)]
a[-(1:5)]
a[a>0]

#Cwiczenie 6. Wykonaj ponizsze operacje.
b=1:10
b[1:5]=0         #wywolaj teraz wektor b
b[c(1,3,5)]=-1   #wywolaj teraz wektor b

#Cwiczenie 7. Na wektorach
x=seq(0,6,by=2)
y=rev(x)             #co robi funkcja "rev"?

#sprawdz dzialania
2-x
x+y
x*y
1/(x+1)+y^2

#Cwiczenie 8.  Dla wektora
x=rep(1:3,2); x
#sprawd? dzia?anie funkcji
max(x)
min(x)
range(x)
length(x)
sort(x)
sum(x)
prod(x)
#Sprawdz tez order(rank), cummax, cummin, which, diff.

#ćwiczenie 9. Dla wektor?w
a=c(6,2,4,6,5,3,9,7,3,1,6)
b=c(1,0,8,4,9,1,6,5,7,2,9)
#sprawdz funkcje
c(a,b)
pmax(a,b)
pmin(a,b)
cbind(a,b)
rbind(a,b)
#oraz
t=cbind(1:4,5:8); t
as.vector(t)

#==========================================================
#ZADANIA A   ZADANIA A   ZADANIA A   ZADANIA A   ZADANIA A
#=========================================================

#Zadanie 1.
#----------
#wartości gestości i dystrybuanty obliczamy funkcja rozpoczynajaca sie odpowiednio od
# "d" (density)
# "p" (probability)
# i dalej skrotu nazwy rozkladu, dla przykladu
#dnorm(x), pnorm(x) - rozklad normalny standardowy
#dnorm(x,mu,sigma), pnorm(x,mu,sigma) -  rozklad normalny o EX=mu i Var(X)=sigma^2
#dexp(x,lambda), pexp(x,lambda) - rozklad wykladniczy z parametrem lambda
#dlnorm(x,mu,sigma), plnorm(x,mu,sigma)- rozklad log-normalny
###########################################################
# P(X < 1) i P(X > 2):
  #a) X ~ N(0, 1)
  pn = pnorm(1); pn;
  pn = 1 - pnorm(2); pn;

  #b) X ~ Exp(1)
  pn = pexp(1); pn;
  pn = 1 - pexp(2); pn;


  #c) X ~ LN(0, 1).
  pn = plnorm(1); pn;
  pn = 1 - plnorm(2); pn;


#Zadanie 2
#---------
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

#b)
  # P(X > 2)
  pn1 <- round(c(1 - pnorm(2), 1 - pexp(2), 1 - plnorm(2)), digits = 2); pn1
  # P(X > 3)
  pn2 <- round(c(1 - pnorm(3), 1 - pexp(3), 1 - plnorm(3)), digits = 2); pn2
  # P(1 < X < 2)
  pn3 <- round(c(pnorm(2) - pnorm(1), pexp(2) - pexp(1), plnorm(2) - plnorm(1)), digits = 2); pn3
  # P(2 < X < 3)
  pn4 <- round(c(pnorm(3) - pnorm(2), pexp(3) - pexp(2), plnorm(3) - plnorm(2)), digits = 2); pn4

#c)
  pn = matrix(c(pn1, pn2, pn3, pn4), nrow = 3, ncol = 4); pn

#Cwiczenie 10. Sprawdz dzialanie petli:
#"for (warunek logiczny) {blok instukcji}"
X=c()
Y=c()                   #wektory w ktorych zapiszemy wyniki
for(i in 1:5){
  X[i]=pnorm(i)
  Y[i]=pnorm(i,1)
  }                     #koniec petli for

round(X,2)              #obliczone wartosci
round(Y,2)

#Zadanie 3. P(X > n) n <- 1:10
#----------
result = c()
for (i in 1:10) {
   pn <- c(1 - pnorm(i), 1 - pexp(i), 1 - plnorm(i))
   result = c(result, pn)
}

m <- matrix(round(result, digits = 2), nrow = 3, ncol = 10, byrow = FALSE); m
m <- matrix(round(result, digits = 2), nrow = 10, ncol = 3, byrow = TRUE); m

#===========================================
#R: podstawowe wykresy
#===========================================
#Cwiczenie 11.
#Na ponizszych przykladach, sprawdz  dzialnie funkcji plot().

#plot()
plot(1:5,c(1,-1,1,-1,1))
plot(1:5,c(1,-1,1,-1,1),ylim=c(-2,2))    #zmieniamy zakres osi oy
plot(1:5,c(1,-1,1,-1,1),ylim=c(-2,2),type='l',col='blue')
                                         #sprawd? typ 'b' oraz 's'
x=c(1:4,rev(1:4))
y=c(3,2,2,3,3,4,4,3)
plot(x,y,type='l',lwd=4,col='red')   #lwd (line width)
plot(x,y,type='l',lwd=4,col='red',xlab=NA,ylab='')  #xlab(labels) - etykiety

x=seq(-2,2,by=0.01)
plot(x,x^2)
plot(x,x^2,type='l')
abline(1,-2)         #abline(a,b) dorysowuje do wykresu proste y=bx+a
abline(h=2,col='red') #h-horizontal, v-vertical

##Cwiczenie 12.
#Na ponizszych przykladach, sprawdz  dzialnie funkcji curve().
curve(x^2,xlim=c(-2,2))
curve(-x^2+3,col='red',add=T)  #add=TRUE pozwala dorysowa? kolejny wykres
curve(sin(4*x)+1.5,lwd=3,lty=3,col='blue',add=T)
                               #lty (line type), sprawd? 0,1,2,3,4

#w przypadkku gestosci, dystrybuant - zamiast pisania wzoru funkcji
#wygodniej jest uzywac gotowych funkcji np. dnorm, pnorm

curve(dnorm, xlim=c(-4,4),
main='wykres gestosci N(0,1)',     #tytul wykresu
col.main='green',                  #kolor tytulu
col.axis='purple',                 #kolor podpisow osi
xlab=NA, ylab="dnorm")              #xlab, ylab - podpisy osi
arrows(2,0.3,1,0.23)               #dodaje wektor o poczatku i koncu
text(2,0.35,'wykres gestosci',col='pink')  #dodaje tekst zaczepiony w punkcie


#==============================================================
#Zadania B    Zadania B    Zadania B    Zadania B    Zadania B
#==============================================================
#Zadanie 4.
#----------



#Przyklad 1
#----------
#Probe losowa z rozkladu generujemy za pomoca funkcji
#rozpoczynajacej sie od "r", a nastepnie  skrotu nazwy rozkladu.

X <- rnorm(100)    #10-elementowa proba z rozkladu N(0,1)
Y <- rexp(200,5)    #20-elementowa proba z rozkladu Exp(5)
Z <- rlnorm(50,0,2) #Z jakiego rozkladu generujemy?
                    #w konsoli napisz: ?rlnorm

hist(X, prob=TRUE)             #histogram
curve(dnorm(x), col=2, add=T)  #gestosc N(0,1)

hist(Y,prob=T)                  #histogram
curve(dexp(x,5), col=2, add=T)  #gestosc Exp(5)

#Zadanie 4.
#----------



#Zadanie 5.
#----------


#Zadanie 6.
#----------



#Zadanie 7.
#----------
