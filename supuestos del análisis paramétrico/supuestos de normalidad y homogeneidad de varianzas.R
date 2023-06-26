# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

## Colocar en memoria las bibliotecas a emplear -------
if(!require(readxl)){install.packages("readxl")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(car)){install.packages("car")}

# Abrir la tabla "sacarosa" -------
sacarosa<-read_excel("sacarosa.xlsx")

# Prueba de normalidad para ph en Lodo -------
lodo<-sacarosa %>% 
  select(muestra,ph) %>% 
  filter(muestra=="Lodo")
shapiro.test(lodo$ph) # ph es normal

# QQ Plot -------
qqnorm(lodo$ph)
qqline(lodo$ph)
qqPlot(lodo$ph)

# Método corto para comparar varianzas -------
# Filtrar para la categoría "Lodo" y "Jugo Claro"
sac<-sacarosa %>%
  select(muestra,ph) %>%
  filter(muestra=="Lodo" | muestra=="Jugo Claro")
head(sac)
tail(sac)
attach(sac)
boxplot(ph~muestra, col="green")
var.test(ph~muestra, data=sac, alternative="two.sided")

## Método largo para comparar las varianzas de Lodo y Jugo Claro
sac1<-sacarosa %>%
  filter(muestra=="Lodo")
sac1
qqPlot(sac1$ph)
ph.lodo<-sacarosa %>%
  select(muestra,ph) %>%
  filter(muestra=="Lodo")
ph.lodo
var.lodo=sd(ph.lodo$ph)^2; var.lodo
ph.jugoc<-sacarosa  %>% 
  select(muestra,ph) %>%
  filter(muestra=="Jugo Claro")
ph.jugoc
var.jugoc=sd(ph.jugoc$ph)^2; var.jugoc
var.test(ph.lodo$ph, ph.jugoc$ph)
