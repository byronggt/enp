# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

## Colocar en memoria las bibliotecas a emplear -------

if(!require(rriskDistributions)){install.packages("rriskDistributions")}
if(!require(car)){install.packages("car")}
if(!require(univariateML)){install.packages("univariateML")}
if(!require(nortest)){install.packages("nortest")}
if(!require(readxl)){install.packages("readxl")}
if(!require(dplyr)){install.packages("dplyr")}

# Abrir la tabla "peces_bentónicos" ------
peces2<-read_excel("peces_bentonicos.xlsx")
attach(peces2)
peces2

# Filtrar para la especie M.surmuletus ------
M.surm<-peces2 %>%
  select(especie,wt) %>% 
  filter(especie=="M.surmuletus")
M.surm
detach(peces2)
attach(M.surm)


# Construir un histograma de frecuencias y
# gráfico de densidad ------

hist(wt, freq=F)
lines(density(wt))
densityPlot(wt)
curve(dnorm(x, mean(wt), sd(wt)), lwd = 2, col = "blue", add = T)

# Verificar el supuesto de normalidad ------
qqPlot(wt)
shapiro.test(wt) 

## Probar el ajuste de otras distribuciones de probabilidad ------

# Crear un objeto nuevo, para probar ajuste de otras distribuciones
wt1<-wt 

# Solicitar los gráficos de QQ plot e hipótesis

res11<-fit.cont(data2fit=wt1) 

# Seleccionar la distribución con el menor AIC. Es decir, el mejor ajuste

summary(wt) # Resumen de los 5 números más la media aritmética
boxplot(wt, col="green", ylab="peso en gramos") # Diagrama de cajas de dispersión 
points(mean(wt), col = 1, pch = 15)

# Gráfica para visualizar el ajuste ------
windows(10,10)
hist(wt,
     main = "Distribución de los pesos de peces de la especie M. Surm.",
     freq = FALSE)
lines(mllnorm(wt), lwd = 2, lty = 1, col = "blue")
legend(x = 250, y = 0.008, legend = c("mllnorm"),
       col = c("blue"), lty = 1:2)
rug(wt)

# Ajuste de una distribución lognormal 
dist1 <- mllnorm(x = wt)
summary(dist1)

# Intervalo de confianza para los estimadores 
# al 95% estimados por bootstrapping
bootstrapml(dist1, probs = c(0.05, 0.95), reps = 1000)
