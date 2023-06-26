# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt
# Prueba del signo -------

# Importar la tabla de datos "niñas" -----

if(!require(readxl)){install.packages("niñas.xlsx")}
n<-read_excel(file.choose())
head(n)
n

# Cálculos mediante la distribución binomial ------

# En el caso de la cola izquierda
# Considerar 9 ensayos, dado que en la fila 2 la resta de la mediana ofrece cero
attach(n)
dif<-calificacion-5;dif

# Ho: La mediana = 5
# Ha: La mediana no es igual a 5
# Considerar el conteo del grupo de signo con el menor valor
# En este caso corresponde a 1 signo negativo y 8 positivos. Elegir 1

# Calcular la probabilidad de cola izquierda para 1 ------

p.izq<-pbinom(c(1), size=9, prob=0.5, lower.tail=TRUE) ; p.izq

# Para una prueba de dos colas se duplica la probabilidad ------

p.2colas<-p.izq*2 ; p.2colas


# Cálculo mediante el paquete BSDA ------

if(!require(BSDA)){install.packages("BSDA")}
SIGN.test(n$calificacion, md=5, alternative = "two.sided", conf.level = 0.95)
SIGN.test(n$calificacion, md=5, alternative = "greater", conf.level = 0.95)
SIGN.test(n$calificacion, md=5, alternative = "less", conf.level = 0.95)