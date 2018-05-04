# TallerNumerico-05-04-2018

6.Implemente un código en R, para este método y obtenga 10 puntos de la solución con h=0.1, grafíquela
y compárela con el método de Euler:

#𝒅𝒚/𝒅𝒙 −x-y−𝟏 + 𝒙^𝟐 = 𝟎; 𝒚(𝟎) = 𝟏

Para solucionar el punto asignado se hizo uso de la siguiente funcion para poder
usar Runge Kutta de cuarto orden, esta funcion tomara el nombre de RK4

       rk4 <- function(f, x0, y0, x1, n) {
        vx <- double(n + 1)
        vy <- double(n + 1)
        vx[1] <- x <- x0
        vy[1] <- y <- y0
        h <- (x1 - x0)/n
        for(i in 1:n) {
          k1 <- h*f(x, y)
          k2 <- h*f(x + 0.5*h, y + 0.5*k1)
          k3 <- h*f(x + 0.5*h, y + 0.5*k2)
          k4 <- h*f(x + h, y + k3)
          vx[i + 1] <- x <- x0 + i*h
          vy[i + 1] <- y <- y + (k1 + k2 + k2 + k3 + k3 + k4)/6
        }
        cbind(vx, vy)
      }
Ademas de crear la funcion se despejo fy/dx para poder utilizar dicha funcion y asi
obteniendo que: 

#dy/dx = x+y+1-x^2.

De esta manera esta funcion se obtienen los 10 primeros datos avanzando de 0.1 en 0.1,
al brindar los datos de x0=0 y y0=1 obtenemos una tabla con los siguientes resultados:

              vx              vy
       [1]    0.0       1.000000
       [2]    0.1       1.215171
       [3]    0.2       1.461402
       [4]    0.3       1.739858
       [5]    0.4       2.051823
       [6]    0.5       2.398719
       [7]    0.6       2.782116
       [8]    0.7       3.203750
       [9]    0.8       3.665537
       [10]   0.9       4.169599
       [11]   1.0       4.718276
       
Seguido a obtener estos datos se uso la funcion de euler para obtener los resultados 
brindados por este metodo, dicho metodo se encuentra representado de la siguiente manera:

       euler <- function(f, x0, y0, h, n) {
         x <- x0
         y <- y0

         for(i in 1:n) {
           y0 <- y0 + h * f(x0, y0)
           x0 <- x0 + h
           x <- c(x, x0)
           y <- c(y, y0)
         }

         return(data.frame(x = x, y = y))
       }

De esta manera obtenemos igualmente una tabla con los datos segun euler para poder 
graficar y comparar ambos metodos, dicha tabla es la siguiente:

                x               y
       [1]      0.0        1.000000
       [2]      0.1        1.200000
       [3]      0.2        1.429000
       [4]      0.3        1.687900
       [5]      0.4        1.977690
       [6]      0.5        2.299459
       [7]      0.6        2.654405
       [8]      0.7        3.043845
       [9]      0.8        3.469230
       [10]     0.9        3.932153
       [11]     1.0        4.434368

Finalmente obtenemos la siguiente grafica donde vemos como Euler cambia de manera mas
notoria luego del 5 dato, la grafica es la siguiente:

