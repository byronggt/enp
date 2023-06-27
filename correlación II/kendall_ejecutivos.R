# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(Kendall)){install.packages("Kendall")}
e1<-c(2,3,1,5,4)
e2<-c(3,1,2,4,5)
Kendall(e1,e2)
summary(Kendall(e1,e2))

# Revisar la aplicación de exactitud y continuidad -------
cor.test(e1,e2,method ="kendall", exact = T,continuity = T)
plot(e1,e2)