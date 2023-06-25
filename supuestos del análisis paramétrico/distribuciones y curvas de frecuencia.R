# Dr. Byron González
# http://cete.fausac.gt 


# Colocar en memoria las bibliotecas a emplear -------
if(!require(readxl)){install.packages("readxl")} # Para importar la tabla de datos
if(!require(RcmdrMisc)){install.packages("RcmdrMisc")}
if(!require(carData)){install.packages("carData")}
if(!require(agricolae)){install.packages("agricolae")}
if(!require(dplyr)){install.packages("dplyr")}

# Importar la tabla de datos "agua1"
agua1<-read_excel("agua1.xlsx")
head(agua1)
attach(agua1)

# Usar la variable potasio para la descripción gráfica
# Construir un histograma de frecuencias absolutas ------
h1<- graph.freq(pot, col="yellow", frequency =1, xlab="Concentración de potasio", ylab="Número de muestras", main="frecuencia absoluta")

# Construir un polígono de frecuencias -------

h2<- graph.freq(pot, frequency =2 , main="Polígono de frecuencia")
polygon.freq(h2, col="blue", lwd=2, frequency =2)

# Construir un gráfico de densidad de probabilidad
# con adición de la curva de probabilidad normal

h3<- graph.freq(pot, col="brown", frequency =3 , main="Gráfico de densidad")
h4<- graph.freq(pot, col="blue", frequency =3 , main="Densidad normal", density=4)
normal.freq(h4, col="red", lty=4,lwd=2, frequency=3)
dens1<-density(pot)
plot(dens1)
h5<- graph.freq(pot, frequency=1, axes=T)

# En caso se requiera definir el número de clases
# , nclass=7, main="frecuencia con 7 clases")

# Construir una ojiva de Galton del tipo "menor que.." ------

h6<-ogive.freq(h5,axes=T,type="b", xlab="Concentración de potasio", ylab="Fracción de muestras", main="Ojiva de Galton", col="red")
round(table.freq(h5),3)
stat.freq(h5)

# Seleccionar las categorías de ph alto y medio
# para comparar si sus varianzas son iguales

sal1<-agua1 %>%
  select(pot, cat_pH)
pot.alto<-sal1 %>%
  filter(cat_pH=="alto")
pot.medio<-sal1 %>%
  filter(cat_pH=="medio")
var.test(pot.alto$pot,pot.medio$pot)
pot.alt.med<- agua1 %>%
  select(pot,cat_pH) %>%
  filter(cat_pH=="alto" | cat_pH=="medio")
var.test(pot.alt.med$pot~pot.alt.med$cat_pH)  
summary(agua1)

# Solicitar estadístico de resumen para ph 
# para las categorías de ph

numSummary(agua1[,c("pot")], groups=agua1$cat_pH, statistics=c("mean", "sd","var"))
