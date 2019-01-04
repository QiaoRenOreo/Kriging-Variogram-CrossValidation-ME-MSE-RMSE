### R code from vignette source 'D:/Dropbox/Teaching/M12/2016/Day4/M12_Day4_handout.rnw'

###################################################
### code chunk number 1: M12_Day4_handout.rnw:80-84
###################################################
require(gstat)
require(sp)
require(rgdal)
require(maptools)


###################################################
### code chunk number 2: M12_Day4_handout.rnw:90-100
###################################################
getwd()
setwd("D:/Study/Module12/4")
rm(list=ls())
load("jura.rda")

jura.pred <- jura.pred[jura.pred$Rock!="Argovian",]
head(jura.pred)
dim(jura.pred)

jura.val <- jura.val[jura.val$Rock!="Argovian",]
head(jura.val)
dim(jura.val)


###################################################
### code chunk number 3: M12_Day4_handout.rnw:104-112
###################################################
jura.pred.df <- jura.pred
coordinates(jura.pred) <- ~Xloc + Yloc

jura.val.df <- jura.val
coordinates(jura.val) <- ~Xloc + Yloc

jura.grid.df <- jura.grid
coordinates(jura.grid) <- ~Xloc + Yloc


###################################################
### code chunk number 4: M12_Day4_handout.rnw:124-127
###################################################
ni.ev <- variogram(Ni~1, data=jura.pred)
ni.mv <- fit.variogram(ni.ev,  model=vgm(70, "Sph", 1, 15), fit.method=7)
plot(ni.ev, ni.mv)


###################################################
### code chunk number 5: M12_Day4_handout.rnw:137-140
###################################################
Y0     <- SpatialPoints(data.frame("Xloc"=2.6,"Yloc"=3.6))
Y0.ok  <- krige(Ni~1, loc=jura.pred, newdata=Y0, model=ni.mv)
Y0.ok


###################################################
### code chunk number 6: M12_Day4_handout.rnw:150-154
###################################################
grid  <- expand.grid(Xloc=seq(0.3, 5.1, by=0.05), Yloc=seq(0.1, 5.9, by=0.05))
grid  <- SpatialPoints(grid)
gridded(grid)  <- TRUE
head(grid)


###################################################
### code chunk number 7: M12_Day4_handout.rnw:158-160
###################################################
plot(grid, axes=T)
plot(jura.pred, col='red', add=T)


###################################################
### code chunk number 8: M12_Day4_handout.rnw:164-165
###################################################
jura.ok  <- krige(Ni~1, loc=jura.pred, newdata=grid, model=ni.mv)


###################################################
### code chunk number 9: M12_Day4_handout.rnw:170-176
###################################################
X11()
spplot(jura.ok, "var1.pred", sp.layout=list("sp.points", pch=19, col="green", jura.pred), 
main="Kriged predictions of Ni (Jura)")
X11()
spplot(jura.ok, "var1.var", sp.layout=list("sp.points", pch=19, col="green", jura.pred), 
main="Kriging variance of Ni (Jura)")


###################################################
### code chunk number 10: M12_Day4_handout.rnw:194-196
###################################################
jura.cv <- krige.cv(Ni~1, jura.pred, model=ni.mv)
str(jura.cv)


###################################################
### code chunk number 11: M12_Day4_handout.rnw:200-203
###################################################
me <- sum(jura.cv$residual) / length(jura.cv$residual)
mse <- sum(jura.cv$residual^2) / length(jura.cv$residual)
rmse <- sqrt(mse)


###################################################
### code chunk number 12: M12_Day4_handout.rnw:209-215
###################################################
jura.val.ok  <- krige(Ni~1, loc=jura.pred, newdata=jura.val, model=ni.mv)
# Calculate the error, rmse and mean error
jura.err <- jura.val.ok$var1.pred - jura.val$Ni
me <- sum(jura.err) / length(jura.err)
mse <- sum(jura.err^2) / length(jura.err)
rmse <- sqrt(mse)


