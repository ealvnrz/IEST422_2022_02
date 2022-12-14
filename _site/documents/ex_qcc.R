library(qcc)
data(pistonrings)
diameter = with(pistonrings, qcc.groups(diameter, sample))
head(diameter)

# Diagrama de control para la media ----

## Diagrama b?sico

q1 = qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,])

## Diagrama nuevas observaciones

plot(q1, chart.all=FALSE)

## Diagrama sin estad?stica

plot(q1, add.stats=FALSE)

## Diagrama con 99% confianza 

q1 = qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,], confidence.level=0.99)

## L?mites de alerta a 2 desviaciones est?ndar

q1 = qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,], plot=FALSE)
warn.limits = limits.xbar(q1$center, q1$std.dev, q1$sizes, 2)
warn.limits

## Diagrama de control + l?mites de alerta

plot(q1, restore.par = FALSE)
abline(h = warn.limits, lty = 3, col = "chocolate")

# Diagrama de control para el rango ----

## Diagrama b?sico + estad?stica descriptiva

q2 = qcc(diameter[1:25,], type="R")

summary(q2)

## diagrama b?sico de todos los datos + estad?stica descriptiva

q3 = qcc(diameter[1:25,], type="R", newdata=diameter[26:40,])

summary(q3)

# Diagrama de control para la desviaci?n est?ndar ----

## Diagrama b?sico + estad?stica descriptiva

q4 = qcc(diameter[1:25,], type="S")

summary(q4)

# Diagrama b?sico de todos los datos + estad?stica descriptiva

q5 = qcc(diameter[1:25,], type="S", newdata=diameter[26:40,])

summary(q5)

# Diagrama de control para tama?os de muestra variables ----

## Quitando datos

out = c(9, 10, 30, 35, 45, 64, 65, 74, 75, 85, 99, 100)
diameter2 = with(pistonrings, qcc.groups(diameter[-out], sample[-out]))

## Diagrama b?sico para la media + estad?stica descriptiva 

summary(qcc(diameter2[1:25,], type="xbar"))

## Diagrama b?sico para el rango + estad?stica descriptiva 

summary(qcc(diameter2[1:25,], type="R"))

# Diagramas de causa y efecto ----

cause.and.effect(cause = list(Measurements = c("Micrometers", "Microscopes", "Inspectors"),
                             Materials    = c("Alloys", "Lubricants", "Suppliers"),
                             Personnel    = c("Shifts", "Supervisors", "Training", "Operators"),
                             Environment  = c("Condensation", "Moisture"),
                             Methods      = c("Brake", "Engager", "Angle"),
                             Machines     = c("Speed", "Lathes", "Bits", "Sockets")),
                effect = "Surface Flaws")

# Diagramas de Pareto ----

defect = c(80, 27, 66, 94, 33)
names(defect) = c("price code", "schedule date", "supplier code", "contact num.", "part num.")
pareto.chart(defect, ylab = "Error frequency")


## Con qicharts ----

library(qicharts)
x <- rep(LETTERS[1:9], c(256, 128, 64, 32, 16, 8, 4, 2, 1))
paretochart(x)


