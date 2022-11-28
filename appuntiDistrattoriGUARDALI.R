# prove distrattori ---- 

source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("provaDIstrattori.R")

simple_m <-apply(Raven(st1=cof(e.hexagon()),
                    hrule=c("fill"),vrule=c("identity")))
draw(simple_m, hide = TRUE)


# ti fa apparire un warning dicendoti quali sono le robe uguali alla risposta 
p = responses(simple_m)
# per il momento non hai controllo sul tipo di distrattori che fai 
# a meno che tu non abbia la pazienza di usare le funioni singole e crearli 
# uno a uno 

select.p = c("correct", "r.diag", "wp.copy", "d.union", 
             "ic.scale")

if (length(select.p) == 5) {
  par(mfrow = c(1, 5))
} else {
  par(mfrow = c(2, 4))
}

select.p = sample(select.p)
for (i in 1:length(select.p)) {

  draw(p[[select.p[i]]], main = select.p[[i]])
}

# prove codice che boh daje così dc

p1 = p
for (i in 1:length(p1)) {
  if (any(names(p1[[i]]) == "attention") == T) {
    p1[[i]] = NULL
  } 
}

draw(p$correct)
draw(p$r.top)

