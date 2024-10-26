# rpois(x,y) números aleatorios de una distribución poison
# set.seed(x) valor semilla; x = valor entero
# x = cantidad de datos a simular; y = el valor promedio (media)

set.seed(1)
x<-rpois(20,4)
x
x[1:5]
mean(x)
median(x)
x[1:5]<-100 #Reemplazando los primeros  datos
x
mean(x)
median(x)
plot(x) #Generación de Grafico
