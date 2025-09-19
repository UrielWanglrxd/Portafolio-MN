---
title: "Sesion MN 03/09"
output: html_document
date: "2025-09-03"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r cars}
x1 <-c(1.2,1-5,1-9,1-10)
(x1) 
```

```{r}
x2 <-1:20
(x2) 
```

```{r}
x3 <- seq(1,20 , by=0.1)
(x3) 
```

```{r}
x4 <- seq(20,1 , by=-0.1)
(x4) 
```

```{r}
x5<- rep(1,times=10)
(x5) 
```

```{r}
n1 <-length(x1) ; (n1)
n2 <-length(x1) ; (n2)
n3 <-length(x1) ; (n3)
n4 <-length(x1) ; (n4)
n5 <-length(x1) ; (n5)
```

```{r}
x<- runif(100,-3,3)
(x) 
```
 Ejemplo de redondeo y truncamiento
 
```{r}
x<- runif(20,-1,1);
(x) 
xx <- 100*x; (xx)
```

Fijamos la semilla para generar numeros aleatorios

```{r}
set.seed(123)
x<- runif(20,-1,1)
(x)
xx<- 100*x;(xx) 
```
Redondeamos a tres cifras significativas:
```{r}
x_red2<- round(xx,2);
(x_red2) 
x_red3 <- round(xx,3);
(x_red3)
```
Generamos una nueva variable con 5000 datos, con una distribucion uniforme entre-10 y 10, que luego se escalaran a dos digitos con 3 cifras significativas posteriormente obtendremos una muestra de tamaño 500 con el comando sample(datos origen,tamaño_de_la_muestra) y se almanecaran en la variable muestra_1:

```{r}
nx<- runif(5000,-10,10);(nx)
head(nx,10)
tail(nx,10)
nx_red <- round(nx,3)
head(nx_red,15)
tail(nx_red,15)
muestral <- sample(nx,5000) ;
head(muestral,10)
tail(muestral, 10)
     
```

Convertimos todos a numeros positivos
```{r}
muestra2 <- abs(muestral)
head(muestra2,15)
tail(muestra2,15)
```
