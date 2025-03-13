#Solucion Pregunta 1

x <- c(0,1)
fx <- c(0.68, 0.32)

#tabla de prob
cbind(x, fx)

plot(x, fx, pch=16, col="red", ylim=c(0,1))
lines(x, fx, type="h", col="red")

mu <- sum(x*fx)
mu

sigmasq <- sum((x-mu)^2*fx) 
sigmasq

0.68*0.32

x
fx

n <- 43
Y <- function(i){sum(sample(x, n, prob = fx, replace=TRUE))}

#bucle
m <- 400000
muestra <- sapply(1:m, Y)
fi <- table(muestra)/m
fi

#frecuencias relativas 
data.frame(fi, Fi=cumsum(fi))

barplot(fi)

###

#tabla de prob  
data.frame(Y=0:43, 
           Prob=dbinom(0:43, 43, 0.32))

y <- 0:43
fy <- dbinom(y, 43, 0.32)

plot(y, fy, pch=19, col="red")
lines(y, fy, col="red", type="h")


dbinom(13, 43, 0.32) #respuesta

############

#tabla de prob  
y <- 0:44
Pi <- dbinom(y, 44, 0.32)
Fi <- cumsum(df$Prob)

pbinom(16, 44, 0.32)
cbind(y, fi, Fi)

plot(y, Fi, type="s", col="red")

####

x <- 0:24
Pi <- dbinom(x, 24, 0.68)

mu <- sum(x*Pi)
24*0.68

sum((x-mu)^2*Pi)
24*0.68*0.32

Fi <- cumsum(Pi)
plot(x, Fi, type="s", col="red")

qbinom(0.25, 24, 0.68)


############################
#Solucion Pregunta 1

#ensayo de Bernoulli

x <- c(0, 1)
fx <- c(0.68, 0.32)

#tabla

cbind(x, fx)
plot(x, fx, ylim=c(0,1), type="h", 
     col="red")
points(x, fx, pch=16, col="red")

n <- 400000
muestra <- sample(x, n, fx, replace = TRUE)
fi <- table(muestra)/n
br <- barplot(fi, ylim=c(0,1)) # frequencias relativas

lines(br, fx, ylim=c(0,1), type="h", 
     col="red") # function de masa de probabilidad
points(br, fx, pch=16, col="red")

#datos
xbar <- mean(muestra)
xbar

#modelo
mu <- sum(x*fx)
mu

fx[2]

#datos varianza muestral
ssq <- var(muestra)
ssq

#varianza de la funcion de masa de probabilidad
sigmasq <- sum((x-mu)^2*fx)
sigmasq
fx[1]*fx[2] #en bernoulli


n <- 43
set.seed(123)
muestra <- sample(x, n, fx, replace=TRUE)


y <- function(i){sum(sample(x, n, fx, replace=TRUE))}
y(4)

#bucle en R
set.seed(123)
m <- 400000 #encuestas de n=43
encuestas <- sapply(1:m, y)
fi <- table(encuestas)/m
data.frame(fi)

dbinom(13, 43, 0.32)

#tabla de probabilidad
resultados <- 1:43
fy <- dbinom(resultados, 43, 0.32)

tabladeprob <- cbind(resultados, fy)
tabladeprob

### n =44
resultados <- 1:44
fy <- dbinom(resultados, 44, 0.32)
tabladeprob <- cbind(resultados, fy)

plot(resultados, 
     fy, type="h", col="red",
     ylim=c(0,0.2))

Fy <- cumsum(fy)
tabladeprob <- cbind(resultados, 
                     fy, Fy)
tabladeprob 

plot(Fy, type="s", col="red")

pbinom(17, 44, 0.32)

###
resultados <- 1:24
fy <- dbinom(resultados, 24, 0.68)
Fy <- cumsum(fy)
tabladeprob <- cbind(resultados, 
                     fy, Fy)
tabladeprob

mu <- sum(resultados*fy)
mu

simgasq <- sum((resultados-mu)^2*fy)

qbinom(0.25, 24, 0.68)

simgasq

plot(Fy, type="s", col="red")


