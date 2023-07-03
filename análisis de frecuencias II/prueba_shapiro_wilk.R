# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt 

if(!require(car)){install.packages("car")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(readxl)){install.packages("readxl")}

## Importar la tabla de datos "estudiantes_medidas_cardiov"
estud<-read_excel("estudiantes_medidas_cardiov.xlsx")

# Separar los registros para hombres y mujeres
estud_h<-estud %>% 
  select (sexo,talla) %>% 
  filter(sexo=="M")

estud_m<-estud %>% 
  select (sexo,talla) %>% 
  filter(sexo=="F")

# Solicitar QQ Plot para talla de hombres
qqPlot(estud_h$talla)


# Solicitar QQ Plot para talla de mujeres
qqPlot(estud_m$talla)


# Calcular el estadístico de prueba de Shapiro & Wilk
shapiro.test(estud_h$talla)

shapiro.test(estud_m$talla)

