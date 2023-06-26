# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(readxl)){install.packages("readxl")}
if(!require(agricolae)){install.packages("agricolae")}
if(!require(car)){install.packages("car")}

# Importar el archivo "sulfuros" ------
sulf<-read_excel("sulfuros.xlsx")
head(sulf)
str(sulf)
attach(sulf)

# Gráfico de box plot para el sulfuto determinado por los tres laboratorios
plot(laboratorio, sulfuro, 
     col="orange",
     xlab="Laboratorio",
     ylab="Concentración de sulfuro")
laboratorio<-as.factor(laboratorio)
dca<-aov(sulfuro~laboratorio)
summary(dca)

# Prueba de Tukey ------

HSD.test(sulfuro, laboratorio, DFerror = 12 , MSerror = 6.5, alpha = 0.05, console=T)

# Prueba de Scott-Knott ------

if(!require(ScottKnott)){install.packages("ScottKnott")}
sk <- SK(dca, which= "laboratorio",  dispersion="se", sig.level=0.05)
summary(sk)

# Prueba para la homogeneidad de varianzas ------
plot(laboratorio,dca$residuals, col="green")
leveneTest(sulfuro ~ laboratorio)
bartlett.test(sulfuro ~ laboratorio, data= sulf)
bartlett.test(dca$residuals, laboratorio)

# Prueba de normalidad de los residuos ------

qqPlot(dca$residuals, # Un gráfico Cuantil-Cuantil de los residuos
       pch =20, #Forma de los puntos
       main="QQ-Plot de los residuos", # Título principal
       xlab = "Cuantiles teóricos",  # Etiqueta eje x
       ylab="Cuantiles observados de los residuos")
plot(dca, 1)
plot(dca, 2)
plot(dca, 5)
shapiro.test(dca$residuals)

# Independencia de los errores -------

# Ho: Los errores son independientes
# Ha: Los errores no son independientes
durbinWatsonTest(dca)
# En caso de incumplimiento de independencia es posible emplea
# la transformación de Cochrane Orcut
if(!require(orcutt)){install.packages("orcutt")}
dca1<-cochrane.orcutt(dca)
dca1