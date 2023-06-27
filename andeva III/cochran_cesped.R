# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(readxl)){install.packages("readxl")}
if(!require(psych)){install.packages("psych")}
if(!require(RVAideMemoire)){install.packages("RVAideMemoire")}
if(!require(coin)){install.packages("coin")}
if(!require(reshape2)){install.packages("reshape2")}
if(!require(rcompanion)){install.packages("rcompanion")}

# Importar la tabla "cesped" ------
cesped<-read_excel("cesped.xlsx")
head(cesped)

# Los bloques corresponden a los estudiantes
# Ordenar los factores para evitar que R lo haga alfabéticamente
cesped$practica = factor(cesped$practica,
                         levels=unique(cesped$practica))

cesped$respuesta = factor(cesped$respuesta,
                          levels=c("si", "no"))

headTail(cesped)

str(cesped)
summary(cesped)

# Crear una nueva variable numérica con 0 y 1 -------

cesped$respuesta.b1 = as.numeric(cesped$respuesta)
head(cesped)
cesped$respuesta.n = as.numeric(cesped$respuesta) - 1   
head(cesped)

Tabla = xtabs(respuesta.n ~ estudiante + practica,
              data=cesped)
Tabla
xtabs( ~ practica + respuesta,
       data=cesped)

# Calcular el estadístico Q de Cochran -------

cochran.qtest(respuesta ~ practica | estudiante,
              data = cesped)

# Prueba post hoc ------- (REVISAR)
# Ordenar los grupos
Data$Practice = factor(Data$Practice,
                       levels = c("MowHeight", "SoilTest",
                                  "Clippings", "Irrigation"))


### Prueba de pares ordenados de McNemar

PT = pairwiseMcnemar(respuesta~ practica | estudiante,
                     data   = cesped,
                     test   = "permutation",
                     method = "fdr",
                     digits = 3)

PT

# Vista de letras compactas -------

PT = PT$Pairwise

cldList(p.adjust ~ Comparison,
        data       = PT,
        threshold  = 0.05)