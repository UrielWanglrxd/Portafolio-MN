---
title: "10.09.MN Y 17/09 MN Uriel W"
output: html_document
date: "2025-09-10"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r}
Discriminante <- function(a,b,c){
  soluciones<- b^2-4*a*c
  if(soluciones<0){
    resp<- "No hay solucion"
  }else{
    resp<- "Si hay solucion"
  }
  respuesta <- cat (resp,"",soluciones)
  return(respuesta)                
}
```


```{r}
intento1<-Discriminante(2,3,5)
```

```{r}
valores<- c(15,13,8,3,27,-4,-2,10,4)
a<- matrix(valores, nrow=3)
(a)
ta<-matrix(t(valores),nrow=3)
(ta)
valores2 <- c(15,3,-2,13,27,10,8,-4,11)
a2<-matrix(valores2,nrow=3)
b<-c (-3,2,-1)
(b)
b<- matrix(c(-3,2,-1),nrow=3)
ab<- cbind(a,b);
(ab)
ba<- cbind(b,a);
(ba)
abt<- rbind(a,t(b));
(abt)
bta<- rbind(t(b),a);
(bta)
```

```{r}
r1 <- ab[1,]; (r1)
r2 <- ab[2,]; (r2)
r3 <- ab[3,]; (r3)
c1 <- ab[,1]; (c1)
c2 <- ab[,2]; (c2)
c3 <- ab[,3]; (c3)
a11 <- ab[1,1];(a11)
a22 <- ab[2,2]; (a22)
a32 <- ab[3,2]; (a32)
a23 <- ab[2,3]; (a23)
a14 <- ab[1,4]; (a14)
a24 <- ab[2,4]; (a24)
```

```{r}
m1 <- ab[2,1]/ab[1,1];(m1)
m2 <- ab[3,1]/ab[1,1];(m2)
a1 <- ab[1,]; (a1)
a2 <- m1*ab[1,]+ab[2,];(a2)
a3 <- m2*ab[1,]+ab[3,];(a3)
```

```{r}
nuevaA <- rbind(a1,a2,a3); (nuevaA)
m3 <- -ab[3,2]/ab[2,2]
a3 <- m3*ab[2,]+ab[3,]; (a3)
a1 <- ab[1,]
a2 <- ab[2,]
NuevaA <- rbind(a1,a2,a3);(NuevaA)
```

## Eliminacion Gaussiana con pivoteo parcial
5x+2y-3z+4w= -1
-x+y+z-3W= 2
6x+6y+4z+w= 0
x-y-z+w= 2
---

## Definición de la matriz aumentada

```{r}
valores <-c(5, 2, -3, 4,
-1, 1, 1, -3,
6, 6, 4, 1,
1, -1, -1, 1)
A=matrix(valores,nrow=4)
A <- t(A)
(A)
b <- c(-1,2,0,2)
```

```{r}
Ab <- cbind(A,b)
(Ab)
```

```{r}
R1 <- A[3,]
R3 <- A[1,]
R2 <- A[2,]
R4 <- A[4,]
NuevaA <- rbind(R1,R2,R3,R4); (NuevaA)
```

```{r}
Ab <- NuevaA; (Ab)
m1 <- -Ab[2,1]/Ab[1,1];(m1)
m2 <- -Ab[1,1]/Ab[1,1];(m2)
m3 <- -Ab[4,1]/Ab[1,1];(m3)
```

```{r}
R1 <- Ab[1,]+Ab[2,];(R2)
R2 <- m1*Ab[1,]+Ab[2,];(R3)
R3 <- m3*Ab[1,]+Ab[3,];(R3)
R4 <- m3*Ab[1,]+Ab[4,];(R4)
```
```{r}
R1 <- NuevaA[1,];
R2 <- NuevaA[3,];
R3 <- NuevaA[2,];
R4 <- NuevaA[4,];
Ab <- rbind(R1,R2,R3,R4); (Ab)
```

```{r}
m4 <- -Ab[3,2]/Ab[2,2];
m5 <- -Ab[4,2]/Ab[2,2];
R3 <- m4*Ab[2,]+Ab[3,];(R3)
R4 <- m5*Ab[2,]+Ab[4,];(R4)
```
```{r}
NuevaA <- rbind(R1,R2,R3,R4); (NuevaA)
```
##Tareita
```{r}
valores<- c(5,-1,6,1,2,1,6,-1,-3,1,-4,-1,4,-3,1,-1,-1,2,0,2)
A<- matrix(valores,nrow = 4)
(A)
```

## Primera eliminación

```{r}
pivoteo<- A[c(3,2,1,4), ]
(pivoteo)
```

##ceros 

```{r}
m1<- -A[2,1]/A[1,1]
m2<- -A[3,1]/A[1,1]
m3<- -A[4,1]/A[1,1]
R1<- pivoteo[1,]
R2<- m1*A[1,]+A[2,]
R3<- m2*A[1,]+A[3,]
R4<- m3*A[1,]+A[4,]
NuevaA<- rbind(R1,R2,R3,R4)
(NuevaA)
```

## Segunda eliminación

## Cambiar columnas
```{r}
pivoteo<- NuevaA[c(1,3,2,4), ]
(pivoteo)
```

##ceros 

```{r}
m4<- -pivoteo[3,2]/pivoteo[2,2]
m5<- -pivoteo[4,2]/pivoteo[2,2]
R1<- pivoteo[1,]
R2<- pivoteo[2,]
R3<- m4*pivoteo[2,]+pivoteo[3,]
R4<- m5*pivoteo[2,]+pivoteo[4,]
NuevaA<- rbind(R1,R2,R3,R4)
(NuevaA)
```
## Tercera eliminación
##ceros 

```{r}
m6<- -NuevaA[4,3]/NuevaA[3,3]
R1<- NuevaA[1,]
R2<- NuevaA[2,]
R3<- NuevaA[3,]
R4<- m6*NuevaA[3,]+NuevaA[4,]
NuevaA<- rbind(R1,R2,R3,R4)
(NuevaA)

```

## Cuarta eliminación)

##ceros 

```{r}
m7<- -NuevaA[3,4]/NuevaA[4,4]
m8<- -NuevaA[2,4]/NuevaA[4,4]
m9<- -NuevaA[1,4]/NuevaA[4,4]

R1<- m9 * NuevaA[4,] + NuevaA[1,]
R2<- m8 * NuevaA[4,] + NuevaA[2,]
R3<- m7 * NuevaA[4,] + NuevaA[3,]
R4<- NuevaA[4,]

NuevaA<- rbind(R1, R2, R3, R4)
(NuevaA)
```

## Quinta eliminación

```{r}
m10<- -NuevaA[2,3]/NuevaA[3,3]
m11<- -NuevaA[1,3]/NuevaA[3,3]

R4<- NuevaA[4,]
R3<- NuevaA[3,]
R2<- m10 * NuevaA[3,] + NuevaA[2,]
R1<- m11 * NuevaA[3,] + NuevaA[1,]

NuevaA<- rbind(R1, R2, R3, R4)
(NuevaA)
```

## Sexta eliminación

```{r}
m12<- -NuevaA[1,2]/NuevaA[2,2]

R4<- NuevaA[4,]
R3<- NuevaA[3,]
R2<- NuevaA[2,]
R1<- m12 * NuevaA[2,] + NuevaA[1,]

NuevaA<- rbind(R1, R2, R3, R4)
(NuevaA)
```

# Soluciones

```{r}
x<- NuevaA[1,5] / NuevaA[1,1]
y<- NuevaA[2,5] / NuevaA[2,2]
z<- NuevaA[3,5] / NuevaA[3,3]
w<- NuevaA[4,5] / NuevaA[4,4]

cat("Soluciones del sistema:\n")
cat("x =", x, "\n")
cat("y =", y, "\n") 
cat("z =", z, "\n")
cat("w =", w, "\n")
```
