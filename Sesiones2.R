#---- SESION 2 ----
##---- DEFINICION DE CONSTANTES ----
e = exp(1); 
x = 0.0034
e <- exp(1)
x <- 0.034;
x0 = e^(2*x)
##---- CONCATENAR Y PEGAR EXPRESIONES ----
txt = "El valor de x0 es _"
cat(txt, x0)
paste(txt,x0)
paste0(txt,x0)
##---- ASIGNACION E IMPRESION ----
x0 <- 1
x1 <- x0 - pi*x0 + 1 
(x1 <- x0 - pi*x0 + 1 ) 
print(x1)
##---- LISTADO DE OBJETOS DEFINIDOS ----
ls()
# Eliminar todos los objetos
rm(list= ls())
ls()
##---- IMPRIMIR PEGAR AVANZADO ----
x0 <- 1
x1 <- x0 - pi*x0 + 1
cat("x0 =", x0, "\n","x1 =", x1) 
##---- EJERCICIOS DE PRACTICA ----
##---- TRABAJO EN CLASE ----
a <- 5
b <- 6
c <- 3
raiz <- ((b^2)-(4*a*c))
disc <- abs(raiz)
abs_disc <- abs(disc)
texto <- "El valor del discriminante es:"
cat(texto,disc)
###---- DISTINTAS MANERAS DE ESCRIBIR----
paste (texto,disc)
paste0(texto,disc)
x0 <- 1
x1 <- x0 - pi*x0 + 1
texto <- "El valor de x0 es:"
paste (texto,x0)
texto <- "El valor de x1 es:"
cat(texto,x1)
x1_2 <- x1^2
x1_3 <- x1^3
x1_4 <- x1^4
texto1 <- "El valor de"
texto2 <- ",el cuadrado es:"
paste(texto1,x1,texto2,x1_2)
ls()
rm(list=ls())
x0 <- 1
x1 <- x0 - pi*x0 + 1
cat("x0 es",x0,"\nx1 es",x1)
cat("x0 es",x0,"\n","x1 es",x1)
###---- FUNCIONES TRIGONOMETRICAS----
a <- 1
b <- 2
c <- 3
alfa <- 35
beta <- 55
gama <- 90
seno_alfa <- a/c
coseno_alfa <- b/c
tangente_alfa <- a/b
texto <- "El valor del seno de alfa es:"
paste(texto,seno_alfa)
texto <- "El valor del coseno de alfa es:"
paste(texto,coseno_alfa)
texto <- "El valor de la tangente de alfa es:"
paste(texto,tangente_alfa)
