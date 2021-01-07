#### código para analizar el plan de vacunación
#### 2021 01 06
#### @rafaelprietoc
require(akima)
require(viridis)

#### En la primera parte, se corren 30,000 simulaciones
#### partiendo de una base diaria de vacinacion y una tasa
#### de vacunación deseada, partiendo de que se requieren
#### 2 vacunas para 130M de personas
####
#### Las figuas están en la segunda parte

Tot   <- 260000000 # total de vacunas: 130M * 2 vacunas
dias  <- 424       # total de días entre febrero 2021 y marzo 2022
# Data frame para correr con distintas tasas y bases
Res   <- data.frame(rate = c(), # % de vacunacion
                    base = c(), # vacunas diarias
                    inc = c(),  # incremento diario
                    M = c(),    # máximo diario
                    fact = c()) # factible
# Para una tasa de vacunación aleatoria (0,1)
# y para una tasa base de vacunación (0, 600000)
# obtener el incremento diario, el máximo y si ese punto
# es factible (es decir, vacunación > 0 para todos los días)
t <- 0:423 
for(k in 1:30000){# correr 30000 simulaciones
  rate <- runif(1)
  base <- runif(1)*600000
  #### incremento para lograr el total de vacunación
  #### partiendo de la base diaria
  inc = 2*(Tot*rate - base * dias) / (dias*(dias-1))
  v <- base + inc * t
  fact = (sum(v>0)==424)*1 # factible si vacunas > 0
  Res <- rbind(Res, data.frame(rate =  rate,
                               base = base,
                               inc = inc,
                               M = max(v),
                               fact = fact))
}

#### correr por las orillas de tasa y base
EdgesX <-c((0:9)/10,
           rep(1, 10),
           (10:1)/10,
           rep(0, 10))
EdgesY <-c(rep(0, 10),
           (0:9)/10,
           rep(1, 10),
           (10:1)/10)
for(sims in 1:length(EdgesX)){
  rate <- EdgesX[sims]
  base <- EdgesY[sims]*600000
  inc = 2*(Tot*rate - base * dias) / (dias*(dias-1))
  v <- base + inc * t
  fact = (sum(v>0)==424)*1
  Res <- rbind(Res, data.frame(rate =  rate,
                               base = base,
                               inc = inc,
                               M = max(v),
                               fact = fact))
}


#### Figuras
#### Número máximo de vacunas en un día
#### x: tasa de vacunación
#### y: base de vacunación (0,1)
cols <- colorRampPalette(c("white", "gold", "tomato","blue", "navyblue", "black"))(700)
fld <- interp(Res$rate, Res$base/max(Res$base), 
              Res$fact*Res$M, 
              nx = 2000, ny = 2000,
              linear = F,
              duplicate = "strip")
fldL <- interp(Res$rate, Res$base/max(Res$base), 
               Res$fact*Res$M, 
               nx = 800, ny = 800,
               linear = T,
               duplicate = "strip")
fld$z[is.na(fld$z)] <- 0
fld$z[fld$z<0] <-0
png("RateBase_Max.PNG", width = 2000, height = 1000)
par(mar = c(0,0,0,0))
image(fld, 
      col = cols,
      main = "",
      xlim = c(0, 1), ylim = c(0, 1),
      xaxt = "n", yaxt = "n", 
      xlab = "",
      ylab = "")
contour(fld, labcex = 1, lwd = 8, 
        col = "white", 
        add = TRUE, 
        levels = 100000*(0:13),
        drawlabels = F) 
dev.off()

#### Incremento diario 
#### x: tasa de vacunación
#### y: base de vacunación (0,1)
cols <- colorRampPalette(c("white", "gray47", "cyan", "steelblue"))(700)
fld <- interp(Res$rate, Res$base/max(Res$base), 
              Res$inc*Res$fact - 2000*(1-Res$fact), 
              nx = 2000, ny = 2000,
              linear = T,
              duplicate = "strip")
fld$z <- fld$z[3:1797,]
fld$z <- fld$z[, 3:1797]
fld$x <- fld$x[3:1797]
fld$y <- fld$y[3:1797]
fld$z[is.na(fld$z)] <- min(fld$z, na.rm = T)
png("RateBase_Incrementos.PNG", width = 2000, height = 1000)
par(mar = c(0,0,0,0))
image(fld, 
      col = cols,
      main = "",
      xlim = c(0, 1), ylim = c(0, 1),
      xaxt = "n", yaxt = "n", 
      xlab = "",
      ylab = "")
contour(fld, labcex = 1, lwd = 8, 
        col = "white", 
        add = TRUE, 
        levels = 400*(-13:13),
        drawlabels = F) 
dev.off()

#### Cobertura lograda
#### x: incremento (0,1)
#### y: base de vacunación (0,1)
cols <- colorRampPalette(c("white", "violetred1", "maroon", "tan4"))
u <- Res$fact == 1
v <- Res$inc >=0
ResF <- Res[u&v,]
fld <- interp(ResF$inc/max(ResF$inc), ResF$base/max(ResF$base),
              ResF$rate*ResF$fact, 
              nx = 2000, ny = 2000,
              linear = T,
              duplicate = "strip")
fld$z <- fld$z[3:1797,]
fld$z <- fld$z[, 3:1797]
fld$x <- fld$x[3:1797]
fld$y <- fld$y[3:1797]
fld$z[is.na(fld$z)] <- min(fld$z, na.rm = T)
png("IncrementoBase_cobertura.PNG", width = 2000, height = 1000)
par(mar = c(0,0,0,0))
image(fld, 
      col = cols(900),
      main = "",
      xlim = c(0, 1), ylim = c(0, 1),
      xaxt = "n", yaxt = "n", 
      xlab = "",
      ylab = "")
contour(fld, labcex = 1, lwd = 8, 
        col = "white", 
        add = TRUE, 
        levels = .10*(0:9),
        drawlabels = F) 
dev.off()
