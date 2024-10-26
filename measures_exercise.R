# -----------------------------------------------------------------------------
#   Estudio: Factores relacionados con enfermedades cardiovasculares
#   Institución: Clínica Tu Salud
#   Tipo de estudio: Transversal
#   Muestra: 1250 pacientes
#   
#   Objetivo:
#   Identificar patrones, relaciones y factores de riesgo que contribuyen
#   al desarrollo de enfermedades cardiovasculares, utilizando métodos 
#   multivariados.
#   
#   Variables:
# -----------------------------------------------------------------------------
#   
#   1. Edad (Discreta)                       : Edad del paciente en años.
#   2. Fuma (Binaria)                        : ¿El paciente es fumador? (1 = Sí, 0 = No).
#   3. Índice de masa corporal (IMC) (Continua) : Medido en kg/m².
#   4. Colesterol total (Continua)           : Nivel de colesterol en mg/dL.
#   5. Glucosa en ayunas (Continua)          : Nivel de glucosa en mg/dL.
#   6. Presión arterial sistólica (Continua) : Medida en mmHg.
#   7. Frecuencia cardíaca en reposo (Continua): Latidos por minuto.
#   8. Gasto calórico diario (Continua)      : Medido en kcal/día.
#   9. Horas de sueño (Continua)             : Número de horas de sueño diarias.
#   10. Consumo semanal de frutas (Discreta) : Porciones de frutas a la semana.
#   11. Control médico regular (Binaria)     : ¿Control anual? (1 = Sí, 0 = No).
#   12. Diabetes (Binaria)                   : Diagnóstico de diabetes (1 = Sí, 0 = No).
#   13. Hipertensión (Binaria)               : Diagnóstico de hipertensión (1 = Sí, 0 = No).
#   14. Antecedentes familiares (Binaria)    : Enfermedades cardiovasculares en la familia (1 = Sí, 0 = No).
#   15. Actividad física (Continua)          : Horas de ejercicio semanal.
#   16. Consumo de alcohol (Ordinal)         : Frecuencia de consumo (1 = Nunca, 2 = Ocasionalmente, 3 = Frecuente).
#   17. Obesidad (Binaria)                   : Obesidad según IMC (1 = Sí, 0 = No).
#   18. Número de visitas al médico (Discreta): Visitas en el último año.
#   19. Género (Nominal)                     : Género del paciente (0 = Masculino, 1 = Femenino).
#   20. Zona de residencia (Politómica)      : Región de residencia (1 = Norte, 2 = Sur, 3 = Centro, 4 = Este).
# -----------------------------------------------------------------------------

library(readxl)
datos<-read_xlsx("data/Tusalud.xlsx")
datos<-data.frame(datos)
head(datos)

######################
# Selección de datos #
######################

datos[c(1,4),] #Selección de pacientes 1 y 4; c = concatenar
datos[1:4,] #Selección de pacientes 1 al 4; : = hasta
datos[-c(1,4),] #Selección de todos los pacientes menos el 1 y 4

#Selección de Variables
datos[,c(1,4)] #Seleccionar variables según posición 1 y 4
datos[,1:4] #Seleccionar variables según posición del 1 al 4

#Selección de variables por el nombre ($)
datos$IMC #Selccionar los valores de la variable IMC
datos$Glucosa #Selccionar los valores de la variable Glucosa
datos[,c("Fuma", "Diabetes")] #concatenación de valores por nombre
names(datos) #Nombre de variables

#Selección de casos y variables
datos[c(101, 110), c(1,4)] #Seleccionar casos 101 y 140 de las variables 1 y 4

# Selcción de casos de acuerdo a criterios
datos[datos$IMC>30,] # Casos con IMC > 30
datos[datos$IMC>30, "Edad"] # Edad de casos con IMC > 30
datos[datos$IMC>30, c("Edad","Genero")] # Edad y Genero de casos con IMC > 30
# Edad, Genero, IMC con IMC > 30 y Edad < 30
datos[datos$IMC>30 & datos$Edad>30, c("Edad","Genero", "IMC")]
# Edad, Genero, IMC con IMC > 30 O Edad < 30
datos[datos$IMC>30 | datos$Edad>30, c("Edad","Genero", "IMC")]

########################
# Medidas Descriptivas #
########################

attach(datos) # Permitir acceder a cada columna directamente

mean(Edad) # Media de la Edad
median(Edad) # Mediana de la Edad
table(Edad) # Frecuencias de Edad
sort(table(Edad), decreasing = TRUE) #Moda de la Edad

# Promedio de IMC por Zona (buscar mayor valor) ¿Cuál es esa zona?
tapply(IMC, Zona, mean) 

#NOTA: tapply() : función para buscar estadisticas descriptivas x grupos

########################
#   Casos de Estudio   #
########################

# 1. Se brindará un descuento especial en atención de obesidad a los pacientes
# de aquella zona en que el promedio de IMC sea mayor. ¿Cuál es la zona?

sort(tapply(IMC, Zona, mean), decreasing = TRUE)[1]

# 2. Se brindará un descuento especial en atención de obesidad a los pacientes
# de aquella zona en que el valor mediano de colesterol sea mayor.
# ¿Cuál es la zona?

sort(tapply(Colesterol, Zona, median), decreasing = TRUE)[1]


# 3. Se brindará un descuento especial en atención de obesidad a los pacientes
# de aquella zona en que los valores de Glucosa en ayunas sean más homogéneos
# ¿Cuál es la zona?


cv <- function(datos){ #Nota: cv = Nombre de la función
  cv<- sd(datos) / mean(datos)
  cv<-100*cv
  cv<-round(cv, 2)
  return(cv)
}

# Coefucuente de variabilidad (cv) = (S // X) * 100; 
# S = Desviación estandar
# X = Promedio 


# Mas Homogeneos = es menor Coefucuente de variabilidad (cv)

sort(tapply(Glucosa, Zona, cv))[1]

# 4. Se brindará un descuento especial en atención de obesidad a los pacientes
# de aquella zona que posea el valor más bajo correspondiente al valor minimo
# del 15% de las edades más altas ¿Cuál es la zona?

sort(tapply(Edad, Zona, quantile, 0.85))[1]

# Interpretación: P85 (Zona2) = 55.05; El 85% de los pacientes de la zona 2
# Tienen una edad maxima de 55.05 años


# 6. Se brindará un descuento especial en atención de obesidad a los pacientes
# de aquella zona en que los valores de de Frecuencia cardiaca se concentren
# en valores altos ¿Cuál es la zona?


#Coeficiente de Asimetría: señala concentración de datos.
# As = 3 ( media - mediana ) / S; S = Desviación Estandar

as <- function(datos){ # Función de Asimetria
  as<- 3 * (mean(datos) - median(datos)) / sd(datos)
  return(as)
}

tapply(FreqCard, Zona, as)
sort(tapply(FreqCard, Zona, as) , decreasing = TRUE)[1]

# Nota: 
# Si coef. de asimetria (As) = 0 entonces La distrib. es Simetrica
# Si coef. de asimetria (As) < 0 entonces La distrib. es Asimetrica positiva
# Si coef. de asimetria (As) > 0 entonces La distrib. es Asimetrica negativa

# Simétrica: Valores centrales
# Asimetrica positiva: Valores bajos
# Asimetrica negativa: Valores altos

#Ejemplos Simetria vs Asimetria (Histogramas)
hist(rnorm(100)) # Distribución Normal = Simetrica
hist(rexp(100)) # Distribución Exponencial = Asimetrica Positiva
hist(1 - rexp(100)) # Distribución Exponencial = Asimetrica Negativa

# aggregate: Compara con mas grupos
aggregate(FreqCard, list(Diabetes, HTA), as) 
# output: Group.1 = Diabetes ; Group.2 = HTA; x = as

aggregate(FreqCard, list(Zona, Genero), as)
# output: Group.1 = Zona ; Group.2 = Genero; x = as

#Añadiendo estadisticas descriptivas
resumen <- aggregate(FreqCard, list(Zona, Genero), as)
media <- aggregate(FreqCard, list(Zona, Genero), mean)$x
resumen <- cbind(resumen, media) # cbind = concatenación de dataframes
names(resumen)<-c("Zona", "Género", "As", "Media") # Cambio de nombres de las variables
resumen

#####################
# Codificar Niveles #
#####################

# Nota: Gestión con variables categoricas.

datos$Fuma<-factor(datos$Fuma, levels = c(0,1), labels = c("No", "Si"))
table(datos$Fuma)

datos$ControlMed<-factor(datos$ControlMed, levels = c(0,1), labels = c("No", "Si"))
table(datos$ControlMed)

datos$Diabetes<-factor(datos$Diabetes, levels = c(0,1), labels = c("No", "Si"))
table(datos$Diabetes)

datos$HTA<-factor(datos$HTA, levels = c(0,1), labels = c("No", "Si"))
table(datos$HTA)

datos$Obesidad<-factor(datos$Obesidad, levels = c(0,1), labels = c("No", "Si"))
table(datos$Obesidad)

datos$Antecedentes<-factor(datos$Antecedentes, levels = c(0,1), labels = c("No", "Si"))
table(datos$Antecedentes)

datos$Alcohol<-factor(datos$Alcohol, levels = 1:3, labels = c("Nunca", "A veces", "Siempre"))
table(datos$Alcohol)

datos$Genero<-factor(datos$Genero, levels = c(0,1), labels = c("Masc", "Fem"))
table(datos$Genero)

datos$Zona<-factor(datos$Zona, levels = 1:4, labels = c("Norte", "Sur", "Centro", "Este"))
table(datos$Zona)


#####################
#    Tabulaciones   #
#####################

detach(datos) #Deshacer attach
attach(datos) #asignar nueuvas etiquetas

table(Zona) # Tabulación Simple

table(Zona, Diabetes) # Tabulación por Zona y Diabetes

table(Zona, Obesidad) # Tabulación por Zona y Obesidad

ftable(Zona, Diabetes, HTA) # Tabulación por Zona, Diabetes y HTA
# Note: ftable: para mas de una variable

