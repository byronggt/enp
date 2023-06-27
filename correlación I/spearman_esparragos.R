# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(readxl)){install.packages("readxl")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(GGally)){install.packages("GGally")}
if(!require(pspearman)){install.packages("pspearman")}

# Importar la tabla "esparragos" -------
esparr<-read_excel("esparragos.xlsx")

# Calcular el coeficiente de correlación de Spearman -------
attach(esparr)
esparr
cor.test(fibra, calidad, method = "spearman", exact = F)

# Seleccionar solo las variables alquitrán y nicotina --------
esparr1<-esparr %>% 
  select(fibra,calidad)
esparr1

# Gráfico de dispersión --------
ggscatmat(esparr1, corMethod = "spearman" )

# Una última manera de calcular el coeficiente de Spearman -------

spearman.test(esparr1$fibra, esparr1$calidad, alternative = "t", approximation = "AS89")
