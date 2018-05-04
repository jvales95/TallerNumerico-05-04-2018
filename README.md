# TallerNumerico-05-04-2018

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
      
Con esta funcion se obtienen los 10 primeros datos avanzando de 0.1 en 0.1, al
brindar los datos de x0=0 y y0=1 obtenemos una tabla con los siguientes resultados

