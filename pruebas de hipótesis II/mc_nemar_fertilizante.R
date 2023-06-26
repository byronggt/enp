# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

# Construir la tabla de contingencia -----
fert<-matrix(c(3,5,25,4), nrow = 2, 
             dimnames = list("Después"=c("si","no"),
                             "Antes"=c("si","no")))
fert

# Calcular el estadístico de prueba de McNemar -------

mcnemar.test(fert,correct = F)
# Cálculo de p-value con la distribución binomial
# b+c = 25+5 = 30
# P(antes) = (8/37)=0.22
# P(después) = (28/37) = 0.76

PXmas25<-1- pbinom(24,30,0.5); PXmas25 

# 0.0001624571 es la probabilidad exacta para rechazar Ho
# con esto se puede afirmar que los agricultores aumentan 
# su interés en adquirir el fertilizante, luego de la 
# demostración, dado que P(después) = 0.76 > 0.22
