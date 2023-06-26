# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

# Fundamentos de distribución normal de probabilidad ------

# Media pob=82 ; desviación estándar pob. =0.50
# Obtener el área entre 83 y 81
P83<-pnorm(c(83), mean=82, sd=0.5, lower.tail=TRUE); P83
P81<-pnorm(c(81), mean=82, sd=0.5, lower.tail=TRUE); P81
areap=P83-P81; areap

# Encontrar un valor de X, a partir de un valor de probabilidad
qnorm(c(0.9772499),mean=82, sd=0.5, lower.tail=TRUE)
qnorm(c(0.02275013),mean=82, sd=0.5, lower.tail=TRUE)
