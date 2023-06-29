# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt 

if(!require(car)){install.packages("car")}

## Generar 200 números aleatorios ------
# Para la distribución normal con media=70 y sd=15
set.seed(1)
normal<-rnorm(200,75,15)
set.seed(1)
weib<-rweibull(200,shape=20, scale=40)
data<-data.frame(normal,weib)
head(data)
tail(data)

# Gráfica de la distribución normal
plot(density(data$normal), main = "Densidad: normal", col="red", lwd=3)

# Gráfica de la distribución de Weibull
plot(density(data$weib), main = "Densidad: Weibull", col="blue", lwd=3)

# QQ plot para normal
qqPlot(data$normal)

# QQ Plot para Weibull
qqPlot(data$weib)

# Calcular el estadístico de prueba de Kolmogorov-Smirnov para normal
ks.test(data$normal, "pnorm", 75,15, alternative = "t", exact = F)

# Calcular el estadístico de prueba de Kolmogorov-Smirnov para Weibull
ks.test(data$weib, "pnorm", 75,15, alternative = "t", exact = F)
