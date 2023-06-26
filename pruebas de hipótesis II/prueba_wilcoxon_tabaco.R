# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(readxl)){install.packages("readxl")}
if(!require(MASS)){install.packages("MASS")}

# Importar la tabla "tabaco"
tabaco<-read_excel("tabaco.xlsx")
head(tabaco)
attach(tabaco)

# Obtener las diferencias entre las muestras ------
dif<-A-B;dif
length(dif[dif>0])
length(dif[dif<0])
if(!require(plyr)){install.packages("plyr")}
with(tabaco,count(sign(dif)))
dif1<-abs(dif);dif1 # Calcular las diferencias en valor absoluto
signo<- ifelse(dif > 0,1,-1); signo # Convertir las diferencias a signos + o -

# Asignas los rangos ------

rango.abs<-rank(dif1, ties.method = "average"); rango.abs # Asignar rangos a las diferencias absolutas
rango<-signo*rango.abs; rango # Devolver el signo a los rangos
tabaco1<-data.frame(A,B,dif,dif1,rango.abs,rango) # Mostrar los resultados en una tabla
tabaco1

# Ho: Las medianas de plantas A = a la mediana de B
# Ha: Las medianas de plantas A no es igual a la mediana de B
wilcox.test(A,B,paired = T, conf.int = 0.95)
# V= igual a la sumatoria de los rangos positivos