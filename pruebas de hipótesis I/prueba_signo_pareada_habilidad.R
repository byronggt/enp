# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(readxl)){install.packages("readxl")}
if(!require(BSDA)){install.packages("BSDA")}

# Importar la tabla de datos "habilidad" ------
habilidad<-read_excel("habilidad.xlsx")
attach(habilidad)
head(habilidad)

# convertir a signos positivos y negativos ------
dif<-despues-antes;dif
if(!require(plyr)){install.packages("plyr")}
with(habilidad,count(sign(dif)))

# Considerar que el interés es verificar si la habilidad
# aumenta con el entrenamiento. Es decir después > antes
# Esto influye en el orden de las variables al solicitar la prueba
# Ho: después <= antes
# Ha: después > antes
SIGN.test(despues,antes,alternative="g", conf.level=0.95)
binom.test(6,11,p=0.5,alternative="g",conf.level=0.95)
pbinom(c(5), size=11, prob=0.5, lower.tail=F)