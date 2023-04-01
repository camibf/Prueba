
#####################
#   Simulaciones    #
#####################

# La dama probando el té

## Código genérico
sample(x = [vector para muestrear], 
       size = [número de muestras a tomar],
       replace = [logical-- Deberían reemplazarse los valores del vector?],
       prob = [vector de ponderaciones de probabilidad])

n_cups <- 8
cups <- sample(rep(c("milk", "tea"), each = n_cups / 2))
cups

guesses <- sample(rep(c("milk", "tea"), each = n_cups / 2))
guesses

cup_results <- cups == guesses
cup_results

n_right <- sum(cup_results)
n_right

# Escribir una función que ejecute una simulación. 

sim_null_tea <- function(n_cups){
  cups <- sample(rep(c("milk", "tea"), each = n_cups / 2))
  guesses <- sample(rep(c("milk", "tea"), each = n_cups / 2))
  cup_results <- cups == guesses
  n_right <- sum(cup_results)
  return(n_right)
}
sim_null_tea(n_cups = 8)

# Ahora, necesitamos ejecutar muchas simulaciones, para ver qué sucede en promedio si ella adivina.

## Código genérico
replicate(n = [número de repeticiones para ejecutar],
          eval = [código para replicar cada vez])

tea_sims <- replicate(5, sim_null_tea(n_cups = 8))
tea_sims

tea_sims <- replicate(1000, sim_null_tea(n_cups = 8))
mean(tea_sims)

quantile(tea_sims, probs = c(0.025, 0.975))

mean(tea_sims == 8)


# Podemos aplicar el código de replicación en diferentes valores de n_cups

n_cups <- seq(from = 2, to = 14, by = 2)
perc_all_right <- sapply(n_cups, FUN = function(n_cups){
  cups_right <- replicate(1000, sim_null_tea(n_cups))
  out <- mean(cups_right == n_cups)
  return(out)
})
perc_all_right


tea_sims <- data_frame(n_cups, perc_all_right)
ggplot(tea_sims, aes(x = n_cups, y = perc_all_right)) + 
  geom_point() + xlab("# de tazas probadas") + 
  ylab("Probabilidad de acertar \nen todas las tazas si adivina")

dhyper(x = [# de tazas que ella supone que tienen leche primero que hacen],
       m = [# de tazas con leche primero],
       n = [# de tazas con té primero],
       k = [# de tazas ella supone que primero tienen leche])

dhyper(x = 3, m = 4, n = 4, k = 4)

dhyper(x = 3, m = 4, n = 4, k = 4) + 
  dhyper(x = 4, m = 4, n = 4, k = 4)


analytical_results <- data_frame(n_cups = seq(2, 14, 2)) %>%
  mutate(perc_all_right = dhyper(x = n_cups / 2,
                                 m = n_cups / 2,
                                 n = n_cups / 2,
                                 k = n_cups / 2))

ggplot(analytical_results, aes(x = n_cups, y = perc_all_right)) + 
  geom_line(color = "darkgray") +
  geom_point(data = tea_sims) + xlab("# de tazas probadas") + 
  ylab("Probabilidad de acertar \nen todas las tazas si adivina")


##  Jugar a los dardos
library("scales")
library("plotrix")
n.throws <- 20
throw.x <- runif(n.throws, min = -1, max = 1)
throw.y <- runif(n.throws, min = -1, max = 1)
head(cbind(throw.x, throw.y))


plot(c(-1, 1), c(-1,1), type = "n", asp=1,
     xlab = "", ylab = "", axes = FALSE)
rect( -1, -1, 1, 1) 
draw.circle( 0, 0, .75, col = "red")
draw.circle( 0, 0, .5, col = "white")
draw.circle( 0, 0, .25, col = "red")
points(throw.x, throw.y, col = "black", pch = 19)

throw.dist <- sqrt(throw.x^2 + throw.y^2)
head(throw.dist)

throw.score <- cut(throw.dist,
                   breaks = c(0, .25, .5, .75, 1.5),
                   labels = c("20", "15", "10", "0"),
                   right = FALSE)
head(throw.score)

table(throw.score)
PROM <- mean(as.numeric(as.character(throw.score)))

5.635 + c(-1, 1) * qnorm(.975) * sqrt(2.27)

n.throws <- 20
n.sims <- 10000

x.throws <- matrix(runif(n.throws * n.sims, -1, 1),
                   ncol = n.throws, nrow = n.sims)
y.throws <- matrix(runif(n.throws * n.sims, -1, 1),
                   ncol = n.throws, nrow = n.sims)
dist.throws <- sqrt(x.throws^2 + y.throws^2)
score.throws <- apply(dist.throws, 2, cut,
                   breaks = c(0, .25, .5, .75, 1.5),
                   labels = c("20", "15", "10", "0"),
                   right = FALSE)

dist.throws[1:3,1:5]
score.throws[1:3,1:5]

mean.scores <- apply(score.throws, MARGIN = 1,
                     function(x){
                             out <- mean(as.numeric(
                                     as.character(x)))
                             return(out)
                     })
head(mean.scores)

mean(mean.scores) ## Theoretical: 5.635

var(mean.scores) ## Theoretical: 2.27

