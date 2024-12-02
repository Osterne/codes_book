
rm(list = ls())

# Amostra

# Estimação dos quantis





# rho(u) = u * (tau - I(u<0))
# S(y) = sum_i^n (rho(yi-y))

yi = rnorm(1000)
tau = 0.25
y = seq(-3, 3, length.out = 1000) # eixo dos quantis

S = function(y, tau){

auxS = 0  

for (j in 1:length(y)) {
  
  u = 0
  for (i in 1:length(yi)) {
    u[i] = yi[i] - y[j]
  }
  
  rho = ifelse(u<0, u*(tau - 1), u*(tau - 0))
  
  auxS[j] = sum(rho)

}
  return(auxS)
}


S(y)

y = seq(-3, 3, length.out = 1000) # eixo dos quantis
plot(y, S(y,tau = 0.25), type = "p", col = "black", xlab = "y", ylab = "S(y)")
lines(y, S(y,tau = 0.50), type = "p", col="red")
lines(y, S(y,tau = 0.75), type = "p", col = "blue")



# Para tau = 0.25
a = S(y,tau = 0.75)
min(a)
y[391]
y[495]
y[613]
summary(yi)










yi = c(-3, -2, -1, 0, 1, 2, 3)
y = -2

ui = yi - y
tau = 0.25

rhoi = ifelse(ui<0, ui*(tau - 1), ui*(tau - 0))

sum(rhoi)

2*0.75
2*0.25



