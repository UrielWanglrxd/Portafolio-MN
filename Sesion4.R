---
title: "SesionMN Ejer"
author: "Uriel W"
date: "2025-09-10"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
Discriminante <- function(a,b,c){
  soluciones <- b^2-4*a*c
  if (soluciones<0){
    resp <- "No hay solucion"
    }else{
      resp <-"Si hay solucion"
    }
  respuesta <- cat(resp," ", soluciones)
  return(respuesta)
}
```

```{r}
intento1<-Discriminante(2,3,5)
```

```{r}
#Matrices
valores <- c(15,13,8,3,27,-4,-2,10,11)
A <- matrix(valores,nrow = 3);print(A)
tA <- matrix(t(valores),nrow = 3);print(tA)
Valores2 <- c(15,3,-2,13,27,10,8,-4,11);print(Valores2)
A2 <- matrix(Valores2,nrow = 3);print(A2)
b <- c(-3,2,-1);print(b)
b <- matrix(c(-3,2,-1),nrow = 3);print(b)
Ab <- cbind(A,b);print(Ab)
bA <- cbind(b,A);print(bA)
Abt <- rbind(A,t(b));print(Abt)
btA <- rbind(t(b),A);print(btA)
```


```{r}
r1 <- Ab[1,];print(r1)
r2 <- Ab[2,];print(r2)
r3 <- Ab[3,];print(r3)
c1 <- Ab[,1];print(c1)
c2 <- Ab[,2];print(c2)
c3 <- Ab[,3];print(c3)
a11 <- Ab[1,1];print(a11)
a22 <- Ab[2,2];print(a22)
a32 <- Ab[3,2];print(a32)
a23 <- Ab[2,3];print(a23)
a14 <- Ab[1,4];print(a14)
a24 <- Ab[2,4];print(a24)
```

```{r}
m1 <- -Ab[2,1]/Ab[1,1];print(m1)
m2 <- -Ab[3,1]/Ab[1,1];print(m2)

A1 <- Ab[1,];print(A1)
A2 <- m1*Ab[1,]+Ab[2,];print(A2)
A3 <- m2*Ab[1,]+Ab[3,];print(A3)
NuevaA <- rbind(A1,A2,A3);print(NuevaA)
```

```{r}
Ab <- NuevaA; print(Ab)
m3 <- -Ab[3,2]/Ab[2,2]; print(m3)
A3 <- m3*Ab[2,]+Ab[3,]; print(A3)
```
