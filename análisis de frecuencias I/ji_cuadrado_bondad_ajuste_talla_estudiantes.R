# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(agricolae)){install.packages("agricolae")}
if(!require(readxl)){install.packages("readxl")}

# Importar la tabla "estudiantes_talla" -------
altura<-read_excel("estudiantes_talla.xlsx")
head(altura)
alt1 <- subset(altura, subset=sexo=="M")
dim(alt1)
head(alt1)
attach(alt1)

# Estimación de la media y desviación estándar de altura -------
media<-mean(talla)
desv<-sd(talla)
summary(alt1)

# Definir los puntos de corte para construir categorías, y
# obtener las frecuencias observadas por categoría
talla.c<-cut(talla, breaks = c(1.45,1.55,1.65,1.75,1.85,1.95))
table(talla.c)
f.obs<-vector()
for(i in 1:5) f.obs[i]<-table(talla.c) [[i]] # Frecuencias observadas
f.obs

# Obtener las frecuencias esperadas
f1<-230*(pnorm(1.55, media, desv))
f2<-230*(pnorm(1.65, media, desv)-pnorm(1.55,media,desv))
f3<-230*(pnorm(1.75, media, desv)-pnorm(1.65,media,desv))
f4<-230*(pnorm(1.85, media, desv)-pnorm(1.75,media,desv))
f5<-230*(1-pnorm(1.85, media, desv))
f.esp<-c(f1,f2,f3,f4,f5); f.esp

# Calcular el estadístico de prueba de Ji cuadrado
# a partir de las frecuencias observadas y esperadas
X2<-sum(((f.obs-f.esp)^2)/f.esp); X2

# Obtener el p-value para gl=5-1=4
p.value<-1-pchisq(X2,4); p.value

# Conclusión: La altura sigue una distribución normal
# Construir el histograma con la curva normal teórica
h<-graph.freq(talla, col="brown", frequency = 3, density=4)
normal.freq(h, col="red", lwd=2, frequency = 3)
